{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Action
    ( Action
    , MixingAction(..)
    , MonadUnderA
    , MonadAction
    , Protocol
    , TransMult

    -- * Restrictions
    , Impure
    , Pure
    , Create
    , Restriction(..)
    , IfImpure
    , ifImpureM
    , ifImpure
    , Ways(..)

    -- * Device access API
    , DeviceHandle
    , DynamicDevice
    , AccuredDevice
    , SubDevice
    , Flist(..)
    , accures
    , unsafeAction
    , pureAction
    , safeAction
    , subDeviceAction

    -- * Device access API
    , devTypeRep
    , withDevice
    , device
    , liftP
    , pureAsync

    -- * Create device API
    , ProtocolConstr(..)
    , CreateCtx
    , addToStage
    , mkDevice
    , mkSubDevice )
where

-- base
import Control.Concurrent

import Data.Typeable
import Data.List

import GHC.Exts
import GHC.Stack

import Unsafe.Coerce

-- async
import Control.Concurrent.Async

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TVar
import Control.Monad.STM

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Reader

-- internal
import Control.Monad.Accum
import Control.Monad.Exception
import Control.Monad.Injectable
import Control.Monad.Regions

import Data.Flist

-- | Like 'Applicative', but without ('<**>') == ap
class Applicative m => MixingAction m where
    (<**>) :: m (a -> b) -> m a -> m b
    (**>)  :: m a -> m b -> m b
    lexpr **> rexpr = (lexpr *> pure id) <**> rexpr

instance MixingAction m => MixingAction (ReaderT r m) where
    rf <**> rv = ReaderT $ \r -> runReaderT rf r <**> runReaderT rv r
    rf **> rv = ReaderT $ \r -> runReaderT rf r **> runReaderT rv r

type Impure      = 'R 'True 'True
type Pure        = 'R 'False 'True
type Create      = 'R 'True 'False
data Restriction = R { remoteMasterAccess :: Bool,  localDeviceAccess :: Bool }

data Ways dev s m a = Ways
    { impureWay :: Action dev Impure s m a
    , pureWay   :: forall eff. IfImpure eff => Action dev eff s m a }

class IfImpure eff where
    ifImpureM :: Ways dev s m a -> Action dev eff s m a
    ifImpure  :: TransMult Monad (Protocol dev) m => a -> a -> Action dev eff s m a

instance {-# OVERLAPPABLE #-} IfImpure eff where
    ifImpureM ways = pureWay ways
    ifImpure  _ v  = pure v

instance {-# OVERLAPS #-} IfImpure Impure where
    ifImpureM ways = impureWay ways
    ifImpure  v _  = pure v

class DeviceHandle devHnd where
    toDynamicDevice :: devHnd dev -> DynamicDevice dev

type MonadUnderA m = (MonadIO m, MonadMask m)
type MonadAction dev m = (MonadUnderA m, HasCallStack, MonadTrans (Protocol dev), MonadUnderA (Protocol dev IO), MonadUnderA (Protocol dev m))

type HandleLock    = TVar (Maybe (Word, ThreadId))

withDevices :: MonadUnderA m => [HandleLock] -> (TVar [HandleLock] -> m a) -> m a
withDevices vars act = do
    tid <- liftIO myThreadId
    let aquireTVar var = do
            val <- readTVar var
            case val of
                Nothing -> writeTVar var (Just (1, tid))
                _       -> retry

        releaseTVar var = do
            val <- readTVar var
            writeTVar var $ case val of
                Just (n, id') | n > 1 -> Just (n - 1, id')
                _                    -> Nothing

    bracket
        (liftIO $ atomically (mapM_ aquireTVar vars) >> newTVarIO vars)
        (liftIO . atomically . (readTVar >=> mapM_ releaseTVar))
        act

data DynamicDevice dev = MkDynamicDevice
    { devicePart   :: forall s. dev s
    , protocolPart :: forall m a. Monad m => Protocol dev m a -> m a
    , handleLock   :: (HandleLock, [HandleLock]) }

instance DeviceHandle DynamicDevice where
    toDynamicDevice = id

newtype AccuredDevice s dev = MkAccuredDevice (DynamicDevice dev)
    deriving DeviceHandle

newtype SubDevice s dev = SubDevice (AccuredDevice s dev)
    deriving DeviceHandle

-- |
-- > accures (dynHnd0 :. dynHnd1 :. dynHnd2 :. Nil) $
-- >     \(staticHnd0 :. staticHnd0 :. staticHnd0 :. _) -> do
-- >         doSomeThingWith staticHnd0
-- >         doSomeThingWith staticHnd1
-- >         doSomeThingWith staticHnd2
accures :: MonadUnderA m => Flist DynamicDevice devs -> (forall s. Flist (AccuredDevice s) devs -> Region s m a) -> m a
accures dynHnds action = region $ regionAct dynHnds action
    where
        regionAct devs f =
            let (hnds, locks) = flistMapFold (\dev -> (MkAccuredDevice dev, uncurry (:) (handleLock dev))) devs
            in withDevices (nub locks) $ const $ f hnds

data ProtocolConstr protocol = ProtocolConstr (forall m a. Monad m => protocol m a -> m a)

type TransMult (cxt :: k -> Constraint) trans (m :: k) = (cxt m, cxt (trans m))

newtype CreateCtx d dev s m = CreateCtx ([d], Action dev Impure s m ())

instance Monad (Protocol dev m) => Monoid (CreateCtx d dev s m) where
    mempty                                    = CreateCtx (mempty, return ())
    mappend (CreateCtx lval) (CreateCtx rval) = CreateCtx (fst lval `mappend` fst rval, snd lval >> snd rval)

runAction :: (Monad m, DeviceHandle devHnd) => devHnd dev -> Action dev eff s m a -> m a
runAction devHnd expr =
    let devHnd' = toDynamicDevice devHnd
    in  protocolPart devHnd' $ runReaderT (unAction expr) $ MkAccuredDevice devHnd'

unsafeAction :: (Monad m, DeviceHandle devHnd) => devHnd dev -> (forall s. Action dev Impure s m a) -> m a
unsafeAction = runAction

safeAction :: Monad m => AccuredDevice s dev -> (forall t. Action dev eff t m a) -> m a
safeAction = runAction

subDeviceAction :: (Monad m, Protocol dev ~ Protocol dev1) => SubDevice s dev -> (forall t. Action dev eff t m b) -> Action dev1 eff s m b
subDeviceAction (SubDevice subDev) = unsafeActionMap (withReaderT (const subDev))

pureAction :: (Monad m, DeviceHandle devHnd) => devHnd dev -> (forall s. Action dev Pure s m a) -> m a
pureAction = runAction

mkDevice :: (Monad (Protocol dev m), MonadIO m, Injectable (Protocol dev)) => m (ProtocolConstr (Protocol dev)) -> (forall s. Action dev eff s (AccumT (CreateCtx HandleLock dev s m) m) (dev s)) -> m (DynamicDevice dev)
mkDevice protocolConstr devConstr = do
    ProtocolConstr unlift    <- protocolConstr
    lock                     <- liftIO $ newTVarIO Nothing
    unlift $ flip runReaderT (MkAccuredDevice $ MkDynamicDevice { protocolPart = unlift }) $ do
        (devRec, CreateCtx (locks, acc)) <- unAction $ injection runAccumT devConstr
        let dynDev = MkDynamicDevice
                { devicePart   = unsafeCoerce devRec
                , protocolPart = unlift
                , handleLock   = (lock, locks) }
        withReaderT (const $ MkAccuredDevice dynDev) $ unAction acc
        return dynDev

mkSubDevice :: (Injectable p, Protocol dev1 ~ p, Protocol dev ~ p)
    => (TransMult Monad p m1, MonadAccum (CreateCtx HandleLock dev1 s m1) m, TransMult MonadIO p m)
    => Action dev Create s (AccumT (CreateCtx HandleLock dev s m1) m) (dev s) -- ^
    -> Action dev1 Create s m (SubDevice s dev)
mkSubDevice devConstr = do
    fakeDev                     <- Action $ asks toDynamicDevice
    lock                        <- liftIO $ newTVarIO Nothing
    (v, CreateCtx (locks, acc)) <- injection runAccumT $ unsafeActionMap (withReaderT (const $ MkAccuredDevice $ MkDynamicDevice { protocolPart = protocolPart fakeDev })) devConstr
    let accuredDev = MkAccuredDevice $ MkDynamicDevice
            { devicePart   = unsafeCoerce v
            , protocolPart = protocolPart fakeDev
            , handleLock   = (lock, locks) }
    accum $ CreateCtx (lock : locks, unsafeActionMap (withReaderT (const accuredDev)) acc)
    return $ SubDevice accuredDev

addToStage :: MonadAccum (CreateCtx d dev s m1) m => Action dev Impure s m1 () -> m ()
addToStage eval = accum $ CreateCtx ([], eval)

type family Protocol (dev :: * -> *) :: (* -> *) -> * -> *

newtype Action (dev :: * -> *) (eff :: Restriction) (s :: *) (m :: * -> *) a = Action { unAction :: ReaderT (AccuredDevice s dev) (Protocol dev m) a }

unsafeActionMap :: Monad m => (ReaderT (AccuredDevice s dev) (Protocol dev m) a -> ReaderT (AccuredDevice s dev1) (Protocol dev1 m) b) -> Action dev eff s m a -> Action dev1 eff s m b
unsafeActionMap f = Action . f . unAction

deriving instance MixingAction (Protocol dev m) => MixingAction (Action dev eff s m)
deriving instance Functor (Protocol dev m)      => Functor (Action dev eff s m)
deriving instance Applicative (Protocol dev m)  => Applicative (Action dev eff s m)
deriving instance Monad (Protocol dev m)        => Monad (Action dev eff s m)
deriving instance MonadIO (Protocol dev m)      => MonadIO (Action dev eff s m)
deriving instance (MonadIO (Protocol dev m), MonadCatch (Protocol dev m)) => MonadCatch (Action dev eff s m)
-- deriving instance MonadThrow (Protocol dev m)   => MonadThrow (Action dev eff s m)
instance MonadIO (Protocol dev m)   => MonadThrow (Action dev eff s m) where
    throwM = liftIO . throwM -- throwMWithStack

instance (MonadTrans (Protocol dev), Monad (Protocol dev m), InScope m r) => InScope (Action dev eff s m) r
instance (MonadTrans (Protocol dev), Monad (Protocol dev m), MonadAccum acc m) => MonadAccum acc (Action dev eff s m) where

instance (MonadTrans (Protocol dev), MonadReader r (Protocol dev m), MonadReader r m) => MonadReader r (Action dev eff s m) where
    ask     = lift ask
    local f = Action . mapReaderT (local f) . unAction

instance (MonadIO (Protocol dev m), MonadMask (Protocol dev m)) => MonadMask (Action dev eff s m) where
    mask eval                = Action $ mask $ \u -> unAction (eval (Action . u . unAction))
    uninterruptibleMask eval = Action $ uninterruptibleMask $ \u -> unAction (eval (Action . u . unAction))

instance MonadTrans (Protocol dev) => MonadTrans (Action dev eff s) where
    lift = Action . ReaderT . const . lift

instance Injectable (Protocol dev) => Injectable (Action dev eff s) where
    injection f  = Action . mapReaderT (injection f) . unAction
    safeMap f    = Action . mapReaderT (safeMap f) . unAction
    withTrans f  = Action . mapReaderT (withTrans f) . unAction

liftP :: Protocol dev m a -> Action dev eff s m a
liftP = Action . ReaderT . const

devTypeRep :: (Typeable dev, Monad m, Monad (Protocol dev m)) => Action dev eff s m TypeRep
devTypeRep = Action $ asks (typeOf1 . devicePart . toDynamicDevice)

withDevice :: Monad (Protocol dev m) => (dev s -> Action dev ('R r 'True) s m a) -> Action dev ('R r 'True) s m a
withDevice act = device >>= act

device :: Monad (Protocol dev m) => Action dev ('R r 'True) s m (dev s)
device = Action $ asks (devicePart . toDynamicDevice)

pureAsync :: TransMult MonadIO (Protocol dev) m => Action dev Pure s IO a -> Action dev eff s m (Async a)
pureAsync action = do
    devHnd <- Action ask
    liftIO $ async $ runAction devHnd action
