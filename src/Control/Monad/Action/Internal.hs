module Control.Monad.Action.Internal where

-- base
-- import Control.Exception ( throw )
import Control.Concurrent

import Data.Typeable

-- async
import Control.Concurrent.Async

-- excpetions
import Control.Monad.Catch

-- mtl
import Control.Monad.Reader
import Control.Monad.Writer

-- stm
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- internal
import Control.Monad.Accum

data Ways dev m a = Ways { impureWay :: forall eff. Action dev (Impure eff) m a, pureWay :: forall eff. Action dev eff m a }

class IfImpure eff where
    ifImpureM :: Ways dev m a -> Action dev eff m a
    ifImpure  :: Monad m => a -> a -> Action dev eff m a

instance {-# OVERLAPPABLE #-} IfImpure eff where
    ifImpureM ways = pureWay ways
    ifImpure  _ v  = pure v

instance {-# OVERLAPS #-} IfImpure (Impure eff) where
    ifImpureM ways = impureWay ways
    ifImpure  v _  = pure v

data Impure v deriving Typeable
data Pure deriving Typeable

type Create               = Impure ()
data Raw proto s          = Raw
data New (dev :: * -> *) s = New

type instance Proto (Raw proto) = proto
type instance Proto (New dev)   = Proto dev

data DeviceRuntimeError = DeviceRuntimeError TypeRep String
    deriving (Eq, Show, Typeable)

instance Exception DeviceRuntimeError

addToStage :: MonadAccum (CreateCtx d m1) m => m1 () -> m ()
addToStage eval = accum $ CreateCtx { commit = eval, dependencies = [] }

data CreateCtx d m = CreateCtx { dependencies :: [d], commit :: m () }

instance Monad m => Monoid (CreateCtx l m) where
    mempty      = CreateCtx [] (return ())
    mappend l r = CreateCtx
        { dependencies = dependencies l `mappend` dependencies r
        , commit       = commit l >> commit r }

type CreateAction dev = Action (New dev) (Impure ()) (AccumT (CreateCtx (TVar DeviceAvailability) (Action dev (Impure ()) IO)) IO)

addDepends :: (Monad m1, MonadAccum (CreateCtx d m1) m) => [d] -> m ()
addDepends xs = accum $ mempty { dependencies = xs }

getTypeRep :: (Typeable dev, Monad m) => Action dev eff m TypeRep
getTypeRep = withDevice (return . typeOf1)

deviceError :: (Typeable dev, MonadUnderA m) => String -> Action dev eff m a
deviceError str = getTypeRep >>= \tr -> throwM $ DeviceRuntimeError tr str

mkSubDevice :: (proto ~ Proto dev, proto ~ Proto dev1) => CreateAction dev (dev ()) -> CreateAction dev1 (AccuredDevice () dev)
mkSubDevice action = do
    proto      <- protocol id
    (dev, ctx) <- liftIO $ runAccumT $ runReaderT (unAction action) MkDeviceHandle { handleLockPart = undefined, protocolPart = proto, devicePart = New }
    tvar       <- liftIO $ newTVarIO Free
    lift $ addDepends (tvar : dependencies ctx)
    let devHnd = MkDeviceHandle (tvar, dependencies ctx) proto dev
    liftIO $ unsafeAction devHnd $ commit ctx
    return $ MkAccuredDevice devHnd

mkDevice :: (proto ~ Proto dev) => IO proto -> CreateAction dev (dev ()) -> IO (DeviceHandle dev)
mkDevice protoAction deviceAction = do
    proto      <- protoAction
    (dev, ctx) <- runAccumT $ runReaderT (unAction deviceAction) MkDeviceHandle { handleLockPart = undefined, devicePart = New, protocolPart = proto }
    tvar       <- newTVarIO Free
    let devHnd = MkDeviceHandle (tvar, dependencies ctx) proto dev
    unsafeAction devHnd $ commit ctx
    return devHnd

protocol :: (Monad m, proto ~ Proto dev) => (proto -> a) -> Action dev eff m a
protocol getter = Action $ asks (getter . protocolPart)

localProtocol :: (Monad m, proto ~ Proto dev) => (proto -> proto) -> Action dev eff m a -> Action dev eff m a
localProtocol f = Action . local (\devHnd -> devHnd { protocolPart = f (protocolPart devHnd) }) . unAction

pureAsync :: MonadIO m => Action dev Pure IO a -> Action dev eff m (Async a)
pureAsync = unsafeMapAction $ \mexpr -> liftIO (async mexpr)

data DeviceAvailability = Free | Captured Int ThreadId
    deriving (Eq, Ord, Show, Typeable)

newtype AccuredDevice s dev = MkAccuredDevice { fromScope :: DeviceHandle dev }
    deriving (Eq, Typeable)

instance Eq (DeviceHandle dev) where
    hndl == hndr = fst (handleLockPart hndl) == fst (handleLockPart hndr)

type family Proto (dev :: * -> *)

type family Nominal (dev :: * -> *) where
    Nominal (New dev) = dev
    Nominal dev       = dev

data DeviceHandle dev = MkDeviceHandle
    { handleLockPart :: (TVar DeviceAvailability, [TVar DeviceAvailability])
    , protocolPart   :: Proto dev
    , devicePart     :: dev () }
    deriving Typeable

type MonadUnderA m = (MonadMask m, MonadIO m)

newtype Action dev eff m a = Action { unAction :: ReaderT (DeviceHandle dev) m a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans)

instance (Monad m, MonadAccum w m) => MonadAccum w (Action dev eff m) where
    accum = lift . accum

instance MonadWriter w m => MonadWriter w (Action dev eff m) where
    tell   = lift . tell
    listen = unsafeMapAction listen
    pass   = unsafeMapAction pass

instance MonadReader r m => MonadReader r (Action dev eff m) where
    ask       = lift ask
    local f m = unsafeMapAction (local f) m

withDevice :: Monad m => (forall lock. dev lock -> Action dev eff m a) -> Action dev eff m a
withDevice action = Action (asks devicePart) >>= action

unsafeMapAction :: (Monad m, Monad m1) => (m a -> m1 b) -> Action dev eff m a -> Action dev eff1 m1 b
unsafeMapAction f = Action . mapReaderT f . unAction

withDevices :: MonadUnderA m => [TVar DeviceAvailability] -> (TVar [TVar DeviceAvailability] -> m a) -> m a
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
        (liftIO $ atomically (mapM_ aquireTVar vars) >> newTVarIO vars)
        (liftIO . atomically . (readTVar >=> mapM_ releaseTVar))
        act

forceAlloc :: MonadUnderA m => [TVar DeviceAvailability] -> (TVar [TVar DeviceAvailability] -> m a) -> m a
forceAlloc vars act = do
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
        (liftIO $ atomically (mapM_ aquireTVar vars) >> newTVarIO vars)
        (liftIO . atomically . (readTVar >=> mapM_ releaseTVar))
        act

unsafeAction :: Monad m => DeviceHandle dev -> Action dev (Impure v) m a -> m a
unsafeAction devHnd action = runReaderT (unAction action) devHnd

pureAction :: Monad m => DeviceHandle dev -> Action dev Pure m a -> m a
pureAction devHnd action = runReaderT (unAction action) devHnd
