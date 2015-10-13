module System.Hardware.GSIOC
    ( GSIOC
    , Byte.RawEnv(..)
    , ImmediateDecode(..)
    , BusHandle
    , mkGsiocProtocol
    , atomic
    , openBus
    , openBus'
    , closeBus
    , Byte.buffered
    , Byte.immediate
    , immediateDecodeError
    , immediateEnum
    , Byte.gsiocOverSerialSettings
    , guardImmediate
    , Byte.rawEnwFromHandle )
where

-- base
import Control.Concurrent
-- import Control.Concurrent.MVar

import Control.Exception ( throw )

import Control.Monad

import Data.Word
import Data.List
import Data.IORef
import Data.Typeable

import System.IO

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- transformers
import Control.Concurrent.Utils
import Control.Monad.IO.Class

-- internal
import Control.Monad.Action

-- import System.Hardware.GSIOC.Raw ( GsiocID, gsiocOverSerialSettings )
import qualified System.Hardware.GSIOC.Raw as Byte
-- import System.Hardware.GSIOC.Raw hiding ( immediate', buffered' )

-- data CollisionIdMod = Replace | Error
--     deriving (Show, Typeable)

data CancelTransaction = Cancel
    deriving (Show, Typeable)

data BusHandleClosed = BusHandleClosed
    deriving (Show, Typeable)

instance Exception CancelTransaction
instance Exception BusHandleClosed

data GSIOC = GSIOC
    { gsiocDeviceId       :: (Word8, BusHandle)
    , gsiocCommit         :: forall a. BusHandle -> Word8 -> Byte.Raw a -> IO a
    , gsiocPollingOutrage :: Integer
    , gsiocImmediateSet   :: [(Char, String -> Bool, String)]
    , gsiocBufferedSet    :: [(Char, String -> Bool, String)] }

data BusHandle = BusHandle
    { busState    :: IORef InternalBusState
    , lockedIds   :: TVar [Byte.GsiocID]
    , wokerThread :: ThreadId
    , cmdPipeLine :: Chan (Word8, Cmd) }

data InternalBusState = IBS
    { busCurrentId  :: Word8
    , busGiocConf   :: Byte.RawEnv IO
    , busRecover    :: forall a. IO a -> SomeException -> IO a }

type Responce a = MVar (Either SomeException a)

data Cmd = forall a . Cmd (Byte.Raw a) (Responce a)

mkGsiocProtocol :: BusHandle -> Word8 -> IO GSIOC
mkGsiocProtocol bus gilsonID =
    -- liftIO $ do
    -- state <- readIORef (busState bus)
    -- when (gilsonID `elem` gsiocBusAllocated state) $ error (show gilsonID ++ " already allocated")
    -- atomicWriteIORef ref (state { gsiocBusAllocated = gilsonID:gsiocBusAllocated state })
    -- putMVar mvar currentId
    return GSIOC
        { gsiocDeviceId       = (gilsonID, bus)
        , gsiocCommit         = commit
        , gsiocPollingOutrage = 500 ms
        , gsiocImmediateSet   = []
        , gsiocBufferedSet    = [] }

-- busScheduler :: TVar [(Word8, Cmd)] -> RawEnv IO -> IO a
-- busScheduler queue cfg = evalRaw'' cfg $ loop 128
--     where
--         loop cid = do
--             (gid, Cmd act res) <- liftIO $ atomically $
--                 readTVar queue >>= \xs -> case xs of
--                     []      -> retry
--                     (x:xs') -> writeTVar queue xs' >> return x
--             try (when (cid /= gid) (connectID gid) >> act) >>= void . liftIO . tryPutMVar res
--             loop gid


openBus' :: Byte.RawEnv IO -> IO BusHandle
openBus' env = do
    pip <- newChan
    ibs <- newIORef IBS
            { busCurrentId  = maxBound
            , busGiocConf   = env
            , busRecover    = \_ e -> throwM e }
    ids <- newTVarIO []

    tid <- forkIO $ forever $ do
            (nid, Cmd act res) <- readChan pip
            v <- try $ {- flip evalWithRecover ibs $ -} do
                state <- readIORef ibs
                tryTakeMVar res >>= maybe (return ()) (\_ -> throwM Cancel)
                v <- Byte.evalRaw'' (busGiocConf state) $ do
                    when (busCurrentId state /= nid) (Byte.connectID nid)
                    act
                modifyIORef' ibs (\st -> st { busCurrentId = nid })
                return v
            void $ tryPutMVar res v

    return BusHandle { wokerThread = tid, lockedIds = ids, cmdPipeLine = pip, busState = ibs }
    where
        _evalWithRecover trans ref = do
            modifyIORef ref (\state -> state { busCurrentId = maxBound })
            state <- readIORef ref
            catch trans (busRecover state trans)

openBus :: Integer -> Handle -> IO BusHandle
openBus gsiocTimeout hnd = openBus' (Byte.rawEnwFromHandle hnd) { Byte.rawTimeout = gsiocTimeout }

closeBus :: BusHandle -> IO ()
closeBus hnd = do
    killThread (wokerThread hnd)
    atomicWriteIORef (busState hnd) (throw BusHandleClosed)

commit :: BusHandle -> Word8 -> Byte.Raw a -> IO a
commit bus id' act = do
    atomically $ readTVar (lockedIds bus) >>= \ids -> check (id' `notElem` ids)
    atomicCommit bus id' act

atomicCommit :: BusHandle -> Word8 -> Byte.Raw a -> IO a
atomicCommit bus id' act = do
    var <- newEmptyMVar
    writeChan (cmdPipeLine bus) (id', Cmd act var)
    (either throw id <$> takeMVar var) `onException` void (tryPutMVar var (Left (SomeException Cancel)))

instance (MonadUnderA m, Proto dev ~ GSIOC) => Byte.MonadGSIOC (Action dev eff m) where
    immediate = immediate'
    buffered  = buffered'

atomic :: (MonadUnderA m, Proto dev ~ GSIOC) => Action dev eff m a -> Action dev eff m a
atomic act = do
    (id', bus) <- protocol gsiocDeviceId
    let lock = liftIO $ atomically $ do
            ids <- readTVar (lockedIds bus)
            guard (id' `notElem` ids)
            writeTVar (lockedIds bus) (id' : ids)
        unlock = liftIO $ atomically $ modifyTVar' (lockedIds bus) (delete id')
    bracket_ lock unlock $ localProtocol (\p -> p { gsiocCommit = atomicCommit }) act

immediate' :: (MonadUnderA m, Proto dev ~ GSIOC) => Char -> Action dev eff m String
immediate' c = do
    (id', bus) <- protocol gsiocDeviceId
    lcommit    <- protocol gsiocCommit
    liftIO $ lcommit bus id' (Byte.immediate c)

buffered' :: (MonadUnderA m, Proto dev ~ GSIOC) => String -> Action dev eff m ()
buffered' cmd = do
    (id', bus) <- protocol gsiocDeviceId
    lcommit    <- protocol gsiocCommit
    liftIO $ lcommit bus id' (Byte.buffered cmd)

data ImmediateDecode
    = ImmediateDecode TypeRep Char String String
    deriving (Eq, Ord, Show, Typeable)

instance Exception ImmediateDecode

immediateDecodeError :: (MonadUnderA m, Typeable dev, Proto dev ~ GSIOC) => Char -> String -> String -> Action dev eff m a
immediateDecodeError cmd responce vars = withDevice $ \dev -> throwM $ ImmediateDecode (typeOf1 dev) cmd responce vars

immediateEnum :: (MonadUnderA m, Typeable dev, Proto dev ~ GSIOC) => Char -> [(String, a)] -> Action dev eff m a
immediateEnum c dict = Byte.immediate c >>= decodeImmediate c dict

decodeImmediate :: (MonadUnderA m, Typeable dev, Proto dev ~ GSIOC) => Char -> [(String, a)] -> String -> Action dev eff m a
decodeImmediate req dict var
    | Just v <- lookup var dict = return v
    | otherwise                 = immediateDecodeError req var (show $ map fst dict)

guardImmediate :: (MonadUnderA m, Typeable dev, Proto dev ~ GSIOC) => Char -> [String] -> String -> Action dev eff m ()
guardImmediate req vars var
    | var `notElem` vars = immediateDecodeError req var (show vars)
    | otherwise  = return ()
