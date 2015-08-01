module System.Hardware.GSIOC
    ( GSIOC
    , RawEnv(..)
    , BusHandle
    , mkGsiocProtocol
    , openBus
    , openBus'
    , closeBus
    , buffered
    , immediate
    , immediateEnum
    , gsiocOverSerialSettings
    , gsiocRegister
    , rawEnwFromHandle )
where

-- base
import Control.Concurrent
-- import Control.Concurrent.MVar

import Control.Exception

import Control.Monad

import Data.Word
-- import Data.Maybe
import Data.IORef
import Data.Typeable

import System.IO

-- stm
-- import Control.Concurrent.STM.TQueue

-- import Control.Monad.STM

-- transformers
import Control.Concurrent.Utils
import Control.Monad.IO.Class

-- mtl
import Control.Monad.Reader

-- internal
import Control.Monad.Action

-- import System.Hardware.GSIOC.Raw ( GsiocID, gsiocOverSerialSettings )
import qualified System.Hardware.GSIOC.Raw as Raw
import System.Hardware.GSIOC.Raw hiding ( immediate, buffered )

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
    , gsiocPollingOutrage :: Integer
    , gsiocImmediateSet   :: [(Char, String -> Bool, String)]
    , gsiocBufferedSet    :: [(Char, String -> Bool, String)] }

data BusHandle = BusHandle
    { busState    :: IORef InternalBusState
    , wokerThread :: ThreadId
    , cmdPipeLine :: Chan (Word8, Cmd) }

data InternalBusState = IBS
    { busCurrentId  :: Word8
    , busFileHandle :: Handle
    , busGiocConf   :: RawEnv IO
    , busRecover    :: forall a. IO a -> SomeException -> IO a }

type Responce a = MVar (Either SomeException a)

data Cmd = forall a . Cmd (Raw a) (Responce a)

mkGsiocProtocol :: MonadUnderA m => BusHandle -> Word8 -> Action NotDevice GSIOC Create m GSIOC
mkGsiocProtocol bus gilsonID =
    -- liftIO $ do
    -- state <- readIORef (busState bus)
    -- when (gilsonID `elem` gsiocBusAllocated state) $ error (show gilsonID ++ " already allocated")
    -- atomicWriteIORef ref (state { gsiocBusAllocated = gilsonID:gsiocBusAllocated state })
    -- putMVar mvar currentId
    return GSIOC
        { gsiocDeviceId       = (gilsonID, bus)
        , gsiocPollingOutrage = 500 ms
        , gsiocImmediateSet   = []
        , gsiocBufferedSet    = [] }

openBus' :: RawEnv IO -> IO BusHandle
openBus' env = do
    pip <- newChan
    ibs <- newIORef IBS
            { busCurrentId  = maxBound
            , busGiocConf   = env
            , busRecover    = \trans e -> throw e }

    tid <- forkIO $ forever $ do
            (nid, Cmd act res) <- readChan pip
            v <- try $ {- flip evalWithRecover ibs $ -} do
                state <- readIORef ibs
                tryTakeMVar res >>= maybe (return ()) (\_ -> throw Cancel)
                v <- evalRaw'' (busGiocConf state) $ do
                    when (busCurrentId state /= nid) (connectID nid)
                    act
                modifyIORef' ibs (\s -> s { busCurrentId = nid })
                return v
            void $ tryPutMVar res v

    return BusHandle { wokerThread = tid, cmdPipeLine = pip, busState = ibs }
    where
        evalWithRecover trans ref = do
            modifyIORef ref (\state -> state { busCurrentId = maxBound })
            state <- readIORef ref
            catch trans (busRecover state trans)

openBus :: Integer -> Handle -> IO BusHandle
openBus timeout hnd = openBus' (rawEnwFromHandle hnd) { rawTimeout = timeout }

closeBus :: BusHandle -> IO ()
closeBus hnd = do
    killThread (wokerThread hnd)
    atomicWriteIORef (busState hnd) (throw BusHandleClosed)

commit :: BusHandle -> Word8 -> Raw a -> IO a
commit bus id' act = do
    var <- newEmptyMVar
    writeChan (cmdPipeLine bus) (id', Cmd act var)
    (either throw id <$> takeMVar var) `onException` void (tryPutMVar var (Left (SomeException Cancel)))

immediate :: MonadUnderA m => Char -> Action dev GSIOC eff m String
immediate c = do
    (id', bus) <- protocol gsiocDeviceId
    liftIO $ commit bus id' (Raw.immediate c)

buffered :: MonadUnderA m => String -> Action dev GSIOC eff m ()
buffered cmd = do
    (id', bus) <- protocol gsiocDeviceId
    liftIO $ commit bus id' (Raw.buffered cmd)

gsiocRegister :: CreateRegisterOptions dev GSIOC s e i a
gsiocRegister = defaultRegister { regWait = gsiocWait }
    where
        gsiocWait :: MonadRegister dev GSIOC s e i a m => Action dev GSIOC eff m ()
        gsiocWait = do
            res <- lift ask >>= registerAsk
            case res of
                Current _ -> do
                    t <- protocol gsiocPollingOutrage
                    delay t
                    gsiocWait
                _         -> return ()

data ImmediateDecodeError
    = ImmediateDecodeError TypeRep Char String [String]
    deriving (Eq, Ord, Show, Typeable)

instance Exception ImmediateDecodeError

immediateEnum :: (MonadUnderA m, Typeable dev) => Char -> [(String, a)] -> Action dev GSIOC eff m a
immediateEnum c dict = immediate c >>= decodeImmediate c dict

decodeImmediate :: (MonadUnderA m, Typeable dev) => Char -> [(String, a)] -> String -> Action dev GSIOC eff m a
decodeImmediate req dict var
    | Just v <- lookup var dict = return v
    | otherwise                 = withDevice $ \dev -> throw (ImmediateDecodeError (typeOf1 dev) req var (map fst dict))

guardImmediate :: (MonadUnderA m, Typeable dev) => Char -> [String] -> String -> Action dev GSIOC eff m ()
guardImmediate req vars var
    | var `notElem` vars = withDevice $ \dev -> throw (ImmediateDecodeError (typeOf1 dev) req var vars)
    | otherwise  = return ()
