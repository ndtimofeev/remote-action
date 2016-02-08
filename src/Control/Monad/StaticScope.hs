{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.StaticScope
    ( AccuredDevice
    , AccureT
    , ScopeT
    , Impure
    , Flist(..)
    , accure
    , accures
    , fromScope
    , toScope
    , inScope
    , scopeAction )
where

-- mtl
import Control.Monad.State.Strict

-- stm
import Control.Concurrent.STM.TVar

-- internal
import Control.Monad.Action.Internal

data Flist (p :: (* -> *) -> *) (c :: [* -> *]) where
    Nil  :: Flist p '[]
    (:.) :: p a -> Flist p as -> Flist p (a ': as)

infixr 5 :.

accures :: MonadUnderA m => Flist DeviceHandle devs -> (forall s. Flist (AccuredDevice s) devs -> ScopeT s m a) -> m a
accures devs' action =
    let (locks, accured) = go devs'
    in withDevices locks $ \_ -> unScopeT $ action accured
    where
        go :: Flist DeviceHandle devs -> ([TVar DeviceAvailability], Flist (AccuredDevice s) devs)
        go Nil           = ([], Nil)
        go (dev :. devs) =
            let (locks, accured) = go devs
            in (uncurry (:) (handleLockPart dev) ++ locks, MkAccuredDevice dev :. accured)

newtype AccureT m a = AccureT { unAccureT :: StateT [TVar DeviceAvailability] m a }

newtype ScopeT s m a = ScopeT { unScopeT :: m a }
    deriving (Applicative, Functor, Monad)

accure :: Monad m => AccureT m a -> m a
accure expr = evalStateT (unAccureT expr) []

toScope :: Monad m => DeviceHandle dev -> (forall s. AccuredDevice s dev -> AccureT m a) -> AccureT m a
toScope devHnd action = AccureT $ do
    modify (++ uncurry (:) (handleLockPart devHnd))
    unAccureT $ action $ MkAccuredDevice devHnd

inScope :: MonadUnderA m => ScopeT s m a -> AccureT m a
inScope action = AccureT $ StateT $ \s -> do
    v <- withDevices s $ \_ -> unScopeT action
    return (v, s)

scopeAction :: Monad m => AccuredDevice s dev -> Action dev (Impure s) m a -> ScopeT s m a
scopeAction (MkAccuredDevice devHnd) = ScopeT . unsafeAction devHnd
