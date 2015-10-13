module Control.Monad.Accum ( AccumT, runAccumT, MonadAccum(..) ) where

-- exceptions
import Control.Monad.Catch

-- mtl
import Control.Monad.Writer
import Control.Monad.Reader

instance Monad m => Monoid (m ()) where
    mempty  = return ()
    mappend = (>>)

newtype AccumT w m a = AccumT { unAccumT :: WriterT w m a }
    deriving (Applicative, Functor, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

runAccumT :: AccumT w m a -> m (a, w)
runAccumT eval = runWriterT (unAccumT eval)

class Monad m => MonadAccum w m | m -> w where
    accum :: w -> m ()

instance (Monoid w, MonadReader r m) => MonadReader r (AccumT w m) where
    ask     = lift ask
    local f = AccumT . local f . unAccumT

instance (Monad m, MonadAccum w m) => MonadAccum w (ReaderT r m) where
    accum = lift . accum

instance (Monad m, Monoid w) => MonadAccum w (AccumT w m) where
    accum = AccumT . tell

-- instance (Monad m, MonadAccum w m, Monoid w1) => MonadAccum w (AccumT w1 m) where
--     accum = lift . accum
