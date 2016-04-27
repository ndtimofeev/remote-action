{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}

module Control.Monad.Regions
    ( AncestorRegion
    , Region
    , region
    , InScope( .. )
    , Scope
    , ioscope
    , scope
    , rescope )
where

-- base
import Control.Monad

import Data.IORef

-- excpetions
import Control.Monad.Catch

-- mtl
import Control.Monad.Trans
import Control.Monad.Reader

-- internal
import Control.Monad.Injectable

class AncestorRegion (m1 :: * -> *) (m2 :: * -> *) where

instance AncestorRegion pr cr => AncestorRegion pr (Region s cr)
instance (AncestorRegion pr cr, MonadTrans t) => AncestorRegion pr (t cr)
instance AncestorRegion (Region s cr) (Region s cr)

newtype Region s m a = Region { unRegion :: m a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

instance MonadTrans (Region s) where
    lift = Region

instance Injectable (Region s) where
    injection  f (Region eval) = Region (f eval)
    safeMap    f (Region eval) = Region (f eval)
    withTrans  f (Region eval) = Region (f eval)

region :: (forall s. Region s m a) -> m a
region = unRegion

class Monad m => InScope m r | m -> r where
    onExit :: r () -> m ()

    default onExit :: (InScope m r, MonadTrans t) => r () -> t m ()
    onExit = lift . onExit

instance InScope m r => InScope (ReaderT e m) r
instance InScope m r => InScope (Region s m) r
instance (Monad r, MonadIO m) => InScope (Scope r m) r where
    onExit r = Scope $ do
        ref <- ask
        liftIO $ modifyIORef' ref (r >>)

newtype Scope r m a = Scope { unScope :: ReaderT (IORef (r ())) m a }
    deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow)

instance MonadTrans (Scope r) where
    lift = Scope . lift

instance Injectable (Scope r) where
    injection  f (Scope eval) = Scope $ injection f eval
    safeMap    f (Scope eval) = Scope $ safeMap f eval
    withTrans  f (Scope eval) = Scope $ withTrans f eval

ioscope :: (MonadMask m, MonadIO m) => Scope m m a -> m a
ioscope eval = bracket
    (liftIO $ newIORef $ return ())
    (join . liftIO . readIORef)
    (runReaderT (unScope eval))

scope :: (Monad m, MonadIO (t m), Injectable t, MonadMask (t m)) => t (Scope (t m) m) c -> t m c
scope eval = bracket
    (liftIO $ newIORef $ return ())
    (join . liftIO . readIORef)
    (\ref -> injection_ (flip runReaderT ref . unScope) eval)

rescope :: (Monad m, Monad (t m), MonadIO (t (Scope r m)), Injectable t, MonadMask (t (Scope r m))) => t (Scope (t m) m) c -> t (Scope r m) c
rescope eval = bracket
    (liftIO $ newIORef $ return ())
    (\ref -> liftIO (readIORef ref) >>= withTrans lift)
    (\ref -> safeMap (Scope . withReaderT (const ref) . unScope) eval)
