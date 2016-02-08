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
    injection :: (Monad m, MonadTrans k) => (forall n a. Monad n => k n a -> n a) -> t (k m) b -> t m b

instance Injectable (ReaderT r) where
    injection = mapReaderT

instance Injectable ListT where
    injection = mapListT

instance Injectable (StateT s) where
    injection = mapStateT

instance Monoid w => Injectable (WriterT w) where
    injection = mapWriterT

instance Injectable MaybeT where
    injection = mapMaybeT

instance Injectable IdentityT where
    injection = mapIdentityT

instance Injectable (ExceptT e) where
    injection = mapExceptT

instance Monoid w => Injectable (RWST r w s) where
    injection = mapRWST
