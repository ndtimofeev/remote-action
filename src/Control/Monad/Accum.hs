module Control.Monad.Accum
    ( AccumT
    , runAccumT
    , MAccumT
    , runMAccumT
    , runMAccumT'
    , Finalize
    , evalFinalize
    , MonadAccum(..) )
where

-- base
import Data.IORef

-- exceptions
import Control.Monad.Catch

-- mtl
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader

-- internal
import Control.Monad.Injectable

instance Monad m => Monoid (m ()) where
    mempty  = return ()
    mappend = (>>)

-- | Restricted version 'WriterT'
newtype AccumT w m a = AccumT { unAccumT :: WriterT w m a }
    deriving (Applicative, Functor, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask, Injectable)

runAccumT :: AccumT w m a -> m (a, w)
runAccumT eval = runWriterT (unAccumT eval)

newtype MAccumT w m a = MAccumT { unMAccumT :: ReaderT (IORef w) m a }
    deriving (Applicative, Functor, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask, Injectable)

runMAccumT :: (Monoid w, MonadIO m) => MAccumT w m a -> m (a, w)
runMAccumT eval = do
    ref <- liftIO $ newIORef mempty
    val <- runReaderT (unMAccumT eval) ref
    acc <- liftIO $ readIORef ref
    return (val, acc)

runMAccumT' :: IORef w -> MAccumT w m a -> m a
runMAccumT' ref eval = runReaderT (unMAccumT eval) ref

type Finalize n m = MAccumT (n ()) m


evalFinalize :: (MonadMask (t m), Monad (t m), Injectable t, MonadIO m) => t (Finalize (t m) m) a -> t m a
evalFinalize eval =
    bracket
        (lift $ liftIO $ newIORef mempty)
        (\ref -> join $ lift $ liftIO $ readIORef ref)
        (\ref -> injection_ (\m -> runReaderT (unMAccumT m) ref) eval)

class Monad m => MonadAccum w m | m -> w where
    accum :: w -> m ()

    default accum :: (MonadAccum w m, MonadTrans t) => w -> t m ()
    accum = lift . accum

instance (Monoid w, MonadReader r m) => MonadReader r (AccumT w m) where
    ask     = lift ask
    local f = AccumT . local f . unAccumT

instance (Monad m, MonadAccum w m) => MonadAccum w (ReaderT r m)

instance (MonadIO m, Monoid w) => MonadAccum w (MAccumT w m) where
    accum w = MAccumT $ do
        ref <- ask
        liftIO $ uninterruptibleMask_ $ atomicModifyIORef' ref (\acc -> (acc `mappend` w, ()))

instance (Monad m, Monoid w) => MonadAccum w (AccumT w m) where
    accum = AccumT . tell
