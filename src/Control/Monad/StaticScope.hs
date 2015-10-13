module Control.Monad.StaticScope
    ( AccuredDevice
    , AccureT
    , ScopeT
    , Impure
    , accure
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
