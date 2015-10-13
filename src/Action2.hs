module Action2 where

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Reader

-- internal
import Control.Monad.Accum

data Impure s

data DevHnd dev = DevHnd (dev ()) (forall m a. Protocol dev m a -> m a)
data ProtocolConstr dev = ProtocolConstr (forall m a. Protocol dev m a -> m a)

newtype CreateCtx dev m = CreateCtx ([Int], Action dev (Impure ()) m ())

data New (dev :: * -> *)  s = New

unsafeAction2 :: DevHnd dev -> Action dev eff m a -> m a
unsafeAction2 (DevHnd devRecord unlift) expr = unlift $ runReaderT (unAction expr) devRecord

mkDevice2 :: IO (ProtocolConstr dev) -> Action (New dev) (Impure ()) (AccumT (CreateCtx dev IO) IO) (dev ()) -> IO (DevHnd dev)
mkDevice2 protocolConstr devConstr = do
    ProtocolConstr unlift        <- protocolConstr
    (devRec, CreateCtx (_, acc)) <- runAccumT $ unsafeAction2 (DevHnd New unlift) devConstr
    unsafeAction2 (DevHnd devRec unlift) acc
    return (DevHnd devRec unlift)

type family Protocol (dev :: * -> *) :: (* -> *) -> * -> *
type instance Protocol (New dev) = Protocol dev

newtype Action dev eff m a = Action { unAction :: ReaderT (dev ()) (Protocol dev m) a }

deriving instance Functor (Protocol dev m)     => Functor (Action dev eff m)
deriving instance Applicative (Protocol dev m) => Applicative (Action dev eff m)
deriving instance Monad (Protocol dev m)       => Monad (Action dev eff m)
deriving instance MonadIO (Protocol dev m)     => MonadIO (Action dev eff m)
deriving instance MonadThrow (Protocol dev m)  => MonadThrow (Action dev eff m)
deriving instance MonadCatch (Protocol dev m)  => MonadCatch (Action dev eff m)

instance (MonadTrans (Protocol dev), Monad (Protocol dev m), MonadAccum acc m) => MonadAccum acc (Action dev eff m) where
    accum = lift . accum

instance (MonadTrans (Protocol dev), MonadReader r (Protocol dev m), MonadReader r m) => MonadReader r (Action dev eff m) where
    ask     = lift ask
    local f = Action . mapReaderT (local f) . unAction

instance MonadMask (Protocol dev m) => MonadMask (Action dev eff m) where
    mask eval                = Action $ mask $ \u -> unAction (eval (Action . u . unAction))
    uninterruptibleMask eval = Action $ uninterruptibleMask $ \u -> unAction (eval (Action . u . unAction))

instance MonadTrans (Protocol dev) => MonadTrans (Action dev eff) where
    lift = Action . ReaderT . const . lift

liftP :: Protocol dev m a -> Action dev eff m a
liftP = Action . ReaderT . const

withDevice :: Monad (Protocol dev m) => (forall s. dev s -> Action dev eff m a) -> Action dev eff m a
withDevice act = Action ask >>= act
