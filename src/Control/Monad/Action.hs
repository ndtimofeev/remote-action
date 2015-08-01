module Control.Monad.Action
    -- ( Pure
    -- , AquireArg
    -- , Impure
    -- , Scope
    -- , Mut
    -- , Create
    -- , CreateM
    -- , NotDevice
    -- , MonadUnderA(..)
    -- , BadConstructor(..)
    -- , DeviceHandle
    -- , Action
    -- , mkDevice
    -- , mkSubDevice
    -- , devicesInScope
    -- , withDevice
    -- , getTypeRep
    -- , protocol
    -- , unsafeAction
    -- , TransactionValue(..)
    -- , TransactionState
    -- , MonadRegister
    -- , Register
    -- , registerValue
    -- , transactionState
    -- , stateValue
    -- , withRegister
    -- , newRegister
    -- , newRegisterWithRef
    -- , registerAsk
    -- , registerWrite
    -- , registerWrite'
    -- , registerWait
    -- , registerRead
    -- , deviceAsk
    -- , CreateRegisterOptions(..)
    -- , defaultRegister )
where

-- base
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception ( throw )
import Control.Monad

import Data.Data
import Data.Word
import Data.IORef
import Data.Typeable
import Data.Unique
import Data.List ( delete, nub )
import Data.Maybe ( catMaybes )

import System.IO
import System.IO.Unsafe

-- async
import Control.Concurrent.Async

-- excpetions
import Control.Monad.Catch

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer

-- stm
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- internal
import Control.Concurrent.SigVar

import Control.Monad.Catch.Utils

data Pure        = Pure
data Impure v    = Impure
type Mut         = Impure (Maybe ())
type Create      = Impure ()
data NotDevice s = NotDevice

newtype BadConstructor = BadConstructor TypeRep
    deriving (Eq, Show, Typeable)

newtype OutScope = OutScope TypeRep
    deriving (Eq, Show, Typeable)

data DeviceAvailability = Free | Captured Int ThreadId
    deriving (Eq, Ord, Show, Typeable)

data DeviceHandle dev proto = MkDeviceHandle
    { handleLock        :: (TVar DeviceAvailability, [TVar DeviceAvailability])
    , handleDeviceState :: (dev (), proto) }
    deriving Typeable

instance Eq (DeviceHandle dev proto) where
    hndl == hndr = fst (handleLock hndl) == fst (handleLock hndr)

instance Exception BadConstructor
instance Exception OutScope

type MonadUnderA m = (MonadMask m, MonadIO m)

newtype Action dev proto eff m a = Action { unAction :: ReaderT (dev (), proto) m a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans)

instance MonadReader r m => MonadReader r (Action dev proto eff m) where
    ask       = lift ask
    local f m = mapAction (local f) m

class HeteroDeviceHandles t where
    h :: [TVar DeviceAvailability] -> t

instance HeteroDeviceHandles [TVar DeviceAvailability] where
    h acc = acc

instance HeteroDeviceHandles t => HeteroDeviceHandles (DeviceHandle a b -> t) where
    h acc devHnd = h (acc ++ fst (handleLock devHnd) : snd (handleLock devHnd))

class AquireArg t where
    aquire :: [TVar DeviceAvailability] -> t

instance AquireArg (Scope c -> IO c) where
    aquire acc act = withDevices (nub acc) (runReaderT (runScope act))

instance AquireArg t => AquireArg (DeviceHandle a b -> t) where
    aquire acc devHnd = aquire (acc ++ fst (handleLock devHnd) : snd (handleLock devHnd))

devicesInScope :: AquireArg t => t
devicesInScope = aquire []

withDevices' :: MonadUnderA m => [TVar DeviceAvailability] -> (IORef [TVar DeviceAvailability] -> m a) -> m a
withDevices' vars act = do
    tid <- liftIO myThreadId
    let prepareTVar var = do
            val <- readTVar var
            case val of
                Captured n id'
                    | id' == tid -> return Nothing
                    | n > 0     -> do
                        writeTVar var (Captured (n + 1) id')
                        return (Just id')
                _               -> writeTVar var (Captured 1 tid) >> return Nothing

        releaseTVar var =
            modifyTVar' var $ \v -> case v of
                Captured n id' | n > 1 -> Captured (n - 1) id'
                _                      -> Free

    bracket
        (liftIO $ do
            tids <- atomically $ catMaybes <$> mapM prepareTVar vars
            mapM_ killThread tids
            return tids)
        (const $ liftIO $ atomically $ mapM_ releaseTVar vars)
        (\tids -> do
            undefined)

withDevices :: MonadUnderA m => [TVar DeviceAvailability] -> (IORef [TVar DeviceAvailability] -> m a) -> m a
withDevices vars act = do
    tid <- liftIO myThreadId
    let aquireTVar var = do
            val <- readTVar var
            case val of
                Free -> writeTVar var (Captured 1 tid)
                _    -> retry

        releaseTVar var =
            modifyTVar' var $ \v -> case v of
                Captured n id' | n > 1 -> Captured (n - 1) id'
                _                      -> Free

    bracket
        (liftIO $ atomically (mapM_ aquireTVar vars) >> newIORef vars)
        (liftIO . (readIORef >=> atomically . mapM_ releaseTVar))
        act

newtype Scope a = Scope { runScope :: ReaderT (IORef [TVar DeviceAvailability]) IO a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

newtype CreateM a = Create { runCreate :: WriterT [TVar DeviceAvailability] IO a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

withDevice :: Monad m =>
    (forall lock. dev lock -> Action dev proto eff m a) ->
    Action dev proto eff m a
withDevice action = Action (asks fst) >>= action

getTypeRep :: (Typeable dev, Monad m) => Action dev proto eff m TypeRep
getTypeRep = withDevice (return . typeOf1)

mkSubDevice :: (Typeable dev) => Action dev proto Create CreateM (dev ()) -> Action dev1 proto Create CreateM (DeviceHandle dev proto)
mkSubDevice action = do
    proto <- protocol id
    let typeRep   = typeOf notDevice
        notDevice = throw (BadConstructor typeRep)
    (dev, tvars) <- liftIO $ runWriterT $ runCreate $ runReaderT (unAction action) (notDevice, proto)
    tvar         <- liftIO $ newTVarIO Free
    lift $ Create $ tell (tvar : tvars)
    return (MkDeviceHandle (tvar, tvars) (dev, proto))

mkDevice :: Typeable dev => Action NotDevice proto Create IO proto -> Action dev proto Create CreateM (dev ()) -> IO (DeviceHandle dev proto)
mkDevice protoAction deviceAction = do
    proto        <- runReaderT (unAction protoAction) (NotDevice, error "protocol not protocol")
    let typeRep   = typeOf notDevice
        notDevice = throw (BadConstructor typeRep)
    (dev, tvars) <- runWriterT (runCreate (runReaderT (unAction deviceAction) (notDevice, proto)))
    tvar         <- newTVarIO Free
    return (MkDeviceHandle (tvar, tvars) (dev, proto))

protocol :: Monad m => (proto -> a) -> Action dev proto eff m a
protocol getter = Action $ asks (getter . snd)

unsafeAction :: Monad m => DeviceHandle dev proto -> Action dev proto (Impure v) m a -> m a
unsafeAction devHnd action = runReaderT (unAction action) (handleDeviceState devHnd)

safeAction :: DeviceHandle dev proto -> Action dev proto (Impure v) Scope a -> IO a
safeAction devHnd action = devicesInScope devHnd (scopeAction devHnd action)

scopeAction :: DeviceHandle dev proto -> Action dev proto (Impure v) Scope a -> Scope a
scopeAction devHnd action = do
    list <- Scope ask >>= liftIO . readIORef
    when (fst (handleLock devHnd) `notElem` list) (error "Not in scope")
    unsafeAction devHnd action

pureAction :: Monad m => DeviceHandle dev proto -> Action dev proto Pure m a -> m a
pureAction devHnd action = runReaderT (unAction action) (handleDeviceState devHnd)

pureAsync :: MonadIO m => Action dev proto Pure IO a -> Action dev proto eff m (Async a)
pureAsync = mapAction $ \mexpr -> liftIO (async mexpr)

data TransactionValue e i a = Complete a | Current i | Undefined e
    deriving (Eq, Show, Read, Ord)

type TransactionState a                = TransactionValue a a a
type MonadRegister dev proto s e i a m = (MonadReader (Register dev proto s e i a) m, MonadUnderA m)

data RegisterTransactionFailed = RegisterTransactionFailed TypeRep
    deriving Typeable

data Register dev proto s e i a = Register
    { registerTarget   :: IORef (TransactionState a)
    , registerWaitLock :: (MVar (Either SomeException ()))
    , registerWrite_   :: forall m. MonadRegister dev proto s e i a m => a -> forall v. Action dev proto (Impure v) m ()
    , registerAsk_     :: forall m. MonadRegister dev proto s e i a m => forall eff. Action dev proto eff m (TransactionValue e i a)
    , registerWait_    :: forall m. MonadRegister dev proto s e i a m => forall eff. Action dev proto eff m () }
    deriving Typeable

withRegister :: MonadUnderA m =>
    Register dev proto s e i a ->
    Action dev proto eff (ReaderT (Register dev proto s e i a) m) b ->
    Action dev proto eff m b
withRegister reg = mapAction $ \mexpr -> runReaderT mexpr reg

mapAction :: (Monad m, Monad m1) => (m a -> m1 b) -> Action dev proto eff m a -> Action dev proto eff1 m1 b
mapAction f mval = Action $ ReaderT $ \(dev, proto) -> f (runReaderT (unAction mval) (dev, proto))

registerValue :: MonadIO m => Register dev proto s e i a -> m a
registerValue reg = transactionState reg >>= return . stateValue

stateValue :: TransactionState a -> a
stateValue st = case st of
    Complete val  -> val
    Current val   -> val
    Undefined val -> val

transactionState :: MonadIO m => Register dev proto s e i a -> m (TransactionState a)
transactionState reg = liftIO $ readIORef $ registerTarget reg

registerWrite :: (Eq a, MonadUnderA m) => Register dev proto s e i a -> a -> Action dev proto (Impure v) m ()
registerWrite reg val = registerValue reg >>= \v -> when (v /= val) $ do
    liftIO $ do
        val <- tryTakeMVar (registerWaitLock reg)
        case val of
            Just _ -> putMVar (registerWaitLock reg) (Right ())
            _      -> return ()
    withRegister reg (registerWrite_ reg val)
    liftIO $ do
        atomicWriteIORef (registerTarget reg) (Current val)
        -- readIORef (registerSignal reg) >>= mapM_ (flip writeChan val)

deviceAsk :: MonadUnderA m => Register dev proto s e i a -> Action dev proto eff m (TransactionValue e i a)
deviceAsk reg = withRegister reg (registerAsk_ reg)

registerAsk :: MonadUnderA m => Register dev proto s e i a -> Action dev proto eff m (TransactionValue e i a)
registerAsk reg =
    (transactionState reg >>= \(Complete v) -> return (Complete v)) `failPattern`
        (deviceAsk reg >>= \val ->
            liftIO $ atomicModifyIORef' (registerTarget reg) $
                \target -> case val of
                    Complete _  -> (Complete (stateValue target), val)
                    Undefined _ -> (Undefined (stateValue target), val)
                    _           -> (target, val))

registerWait :: MonadUnderA m => Register dev proto s e i a -> Action dev proto eff m ()
registerWait reg = do
    state <- bracketOnError
        (liftIO $ takeMVar (registerWaitLock reg))
        unlockWith
        (either throwM $ \_ -> return (Right ()) `doPatternFail` do
            Current _ <- transactionState reg
            try (withRegister reg (registerWait_ reg)))
    unlockWith state
    either throwM (const (return ())) state
    where
        unlockWith = liftIO . putMVar (registerWaitLock reg)

registerWrite' :: (Eq a, MonadUnderA m) => Register dev proto s e i a -> a -> Action dev proto (Impure v) m ()
registerWrite' reg val = registerWrite reg val >> registerWait reg

registerRead :: MonadUnderA m => Register dev proto s e i a -> Action dev proto eff m a
registerRead reg = do
    registerWait reg
    error "Bad registor reading" `patternFail` (registerAsk reg >>= \(Complete v) -> return v)

data CreateRegisterOptions dev proto s e i a = CreateRegisterOptions
        { regWait :: forall m. MonadRegister dev proto s e i a m => forall eff. Action dev proto eff m ()
        , regAsk  :: forall m. MonadRegister dev proto s e i a m => forall eff. Action dev proto eff m (TransactionValue e i a) }

defaultRegister :: CreateRegisterOptions dev proto s e i a
defaultRegister =
    CreateRegisterOptions
        { regWait = return () `patternFail` (lift ask >>= registerAsk >>= \(Current _) -> regWait defaultRegister)
        , regAsk  = lift ask >>= registerValue >>= return . Complete }

newRegister :: MonadUnderA m =>
    (forall m. MonadRegister dev proto s e i a m => a -> forall v. Action dev proto (Impure v) m ()) ->
    CreateRegisterOptions dev proto s e i a ->
    TransactionState a ->
    Action dev proto Create m (Register dev proto s e i a)
newRegister mutator params v = liftM fst $ newRegisterWithRef mutator params v

newRegisterWithRef :: MonadUnderA m =>
    (forall m1. MonadRegister dev proto s e i a m1 => a -> forall v. Action dev proto (Impure v) m1 ()) ->
    CreateRegisterOptions dev proto s e i a ->
    TransactionState a ->
    Action dev proto Create m (Register dev proto s e i a, IORef (TransactionState a))
newRegisterWithRef mutator params initVal = liftIO $ do
    ref   <- newIORef initVal
    bar   <- newMVar (Right ())
    return (Register ref bar mutator (regAsk params) (regWait params), ref)
