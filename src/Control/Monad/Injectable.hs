module Control.Monad.Injectable where

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
    injection :: Monad m => (forall n a. Monad n => k n a -> n a) -> t (k m) b -> t m b
    safeMap   :: Monad m => (forall n a. Monad n => k n a -> j n a) -> t (k m) b -> t (j m) b
    withTrans :: Monad m => (forall n a. Monad n => n a -> k n a) -> t m b -> t (k m) b

instance Injectable (ReaderT r) where
    injection = mapReaderT
    safeMap   = mapReaderT
    withTrans = mapReaderT

instance Injectable ListT where
    injection = mapListT
    safeMap   = mapListT
    withTrans = mapListT

instance Injectable (StateT s) where
    injection = mapStateT
    safeMap   = mapStateT
    withTrans = mapStateT

instance Monoid w => Injectable (WriterT w) where
    injection = mapWriterT
    safeMap   = mapWriterT
    withTrans = mapWriterT

instance Injectable MaybeT where
    injection = mapMaybeT
    safeMap   = mapMaybeT
    withTrans = mapMaybeT

instance Injectable IdentityT where
    injection = mapIdentityT
    safeMap   = mapIdentityT
    withTrans = mapIdentityT

instance Injectable (ExceptT e) where
    injection = mapExceptT
    safeMap   = mapExceptT
    withTrans = mapExceptT

instance Monoid w => Injectable (RWST r w s) where
    injection = mapRWST
    safeMap   = mapRWST
    withTrans = mapRWST
