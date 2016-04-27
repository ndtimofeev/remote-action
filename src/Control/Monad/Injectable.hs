module Control.Monad.Injectable ( Injectable(..), injection_ ) where

-- base
import Control.Monad

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

class MonadTrans t => Injectable t where
    injection   :: Monad m => (forall n a. Monad n => k n a -> n (a, c)) -> t (k m) b -> t m (b, c)
    safeMap     :: Monad m => (forall n a. Monad n => k n a -> j n a) -> t (k m) b -> t (j m) b
    withTrans   :: Monad m => (forall n a. Monad n => n a -> k n a) -> t m b -> t (k m) b

instance Injectable (ReaderT r) where
    injection   = mapReaderT
    safeMap     = mapReaderT
    withTrans   = mapReaderT

instance Injectable ListT where
    injection f = mapListT (fmap transformF . f)
    safeMap     = mapListT
    withTrans   = mapListT

instance Injectable (StateT s) where
    injection f = mapStateT (fmap transformP . f)
    safeMap     = mapStateT
    withTrans   = mapStateT

instance Monoid w => Injectable (WriterT w) where
    injection f = mapWriterT (fmap transformP . f)
    safeMap     = mapWriterT
    withTrans   = mapWriterT

instance Injectable MaybeT where
    injection f = mapMaybeT (fmap transformF . f)
    safeMap     = mapMaybeT
    withTrans   = mapMaybeT

instance Injectable IdentityT where
    injection   = mapIdentityT
    safeMap     = mapIdentityT
    withTrans   = mapIdentityT

instance Injectable (ExceptT e) where
    injection f = mapExceptT (fmap transformF . f)
    safeMap     = mapExceptT
    withTrans   = mapExceptT

instance Monoid w => Injectable (RWST r w s) where
    injection f = mapRWST (fmap (\((v1, v2, v3), x) -> ((v1, x), v2, v3)) . f)
    safeMap     = mapRWST
    withTrans   = mapRWST

injection_ :: (Injectable t, Monad (t m), Monad m) => (forall n a. Monad n => k n a -> n a) -> t (k m) b -> t m b
injection_ f m = liftM fst $ injection (fmap (\v -> (v, ())) . f) m

transformF :: Functor f => (f a, b) -> f (a, b)
transformF (fval, val) = fmap (\x -> (x, val)) fval

transformP :: ((a, b), c) -> ((a, c), b)
transformP ((x, y), z) = ((x, z), y)
