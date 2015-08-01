module Control.Monad.Statemachine where

-- base
import Control.Applicative

import Data.IORef
import Data.Typeable

import Data.Functor.Identity

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

-- mtl
import Control.Monad.Reader
import Control.Monad.State.Class

-- internal
import Control.Concurrent.SigVar
import Control.Monad.Action         hiding ( withRegister )

data AbsurdRegisterWait = AbsurdRegisterWait
    deriving (Show, Typeable)

data InvalidTransitionTarget = InvalidTransitionTarget String
    deriving (Show, Typeable)

instance Exception AbsurdRegisterWait
instance Exception InvalidTransitionTarget

type Transition dev proto l eff f r m a = Action dev proto eff (ReaderT (StateM dev proto l f r) m) a

class Result f where
    tryResult :: f a -> Maybe a

instance Result Maybe where
    tryResult = id

instance Result Identity where
    tryResult = Just . runIdentity

instance Result (Either b) where
    tryResult = either (const Nothing) Just

data StateM dev proto l f a = StateM
    { stateVar_         :: TSigVar (Maybe (f a))
    , transitionTarget_ :: TSigVar (Maybe a)
    , readyToStart_     :: TSigVar Bool
    , askTarget_        :: forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (Maybe a)
    , askState_         :: forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a)
    , transitionStart_  :: forall m. MonadUnderA m => a -> forall eff. Transition dev proto l (Impure eff) f a m ()
    , waitState_        :: forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a) }
    deriving Typeable

withStateM :: MonadUnderA m =>
    StateM dev proto l f r -> Transition dev proto l eff f r m b -> Action dev proto eff m b
withStateM reg = mapAction $ flip runReaderT reg

newStateM :: MonadUnderA m =>
    (forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a)) ->
    (forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (Maybe a)) ->
    (forall m. MonadUnderA m => forall eff. a -> Transition dev proto l (Impure eff) f a m ()) ->
    (forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a)) ->
    Maybe (f a) -> Action dev proto Create m (StateM dev proto l f a)
newStateM get target mut waiter mstate = liftIO $ atomically $ do
    st <- newTSigVar mstate
    tg <- newTSigVar Nothing
    rs <- newTSigVar True
    return StateM
        { stateVar_         = st
        , transitionTarget_ = tg
        , readyToStart_     = rs
        , askState_         = get
        , askTarget_        = target
        , transitionStart_  = mut
        , waitState_        = waiter }

defaultTransitionTargetAsk :: MonadUnderA m => Transition dev proto l eff f a m (Maybe a)
defaultTransitionTargetAsk = asks transitionTarget_ >>= readTSigVarIO

invalidTarger :: MonadUnderA m => String -> Transition dev proto l eff f a m b
invalidTarger = throwM . InvalidTransitionTarget

getState :: MonadUnderA m => Transition dev proto l eff f a m (f a)
getState = asks stateVar_ >>= readTSigVarIO >>= maybe askState return

-- | Request current machine state or 'Nothing' if in transition.
inState :: MonadUnderA m => Transition dev proto l eff f a m (Maybe (f a))
inState = asks stateVar_ >>= readTSigVarIO

getTarget :: MonadUnderA m => Transition dev proto l eff f a m (Maybe a)
getTarget = ask >>= \st -> runMaybeT $ do
    MaybeT (readTSigVarIO $ transitionTarget_ st) <|> MaybeT (askTarget_ st)

askState :: MonadUnderA m => Transition dev proto l eff f a m (f a)
askState = ask >>= askState_

simpleStatusRequest :: (Result f, Eq (f a), Eq a, MonadUnderA m) =>
    (forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a)) ->
    Transition dev proto l eff f a m (f a)
simpleStatusRequest request = do
    status  <- request
    runMaybeT $ do
        res <- MaybeT $ return $ tryResult status
        t   <- MaybeT getTarget
        when (res == t) $ lift $ commitState status
    return status

simpleMut :: (Eq (f a), Eq a, MonadUnderA m) =>
    (forall m. MonadUnderA m => forall eff. a -> Transition dev proto l (Impure eff) f a m ()) ->
    a -> Transition dev proto l (Impure eff) f a m ()
simpleMut mut v = mut v >> beginTransition v

-- | The simplest strategy of waiting for a new state: nonstop polling.
simpleWait :: MonadUnderA m => Transition dev proto l eff f a m (f a)
simpleWait = askState >> inState >>= maybe simpleWait return

sharedWaiter :: MonadUnderA m =>
    TVar Bool ->
    (forall m. MonadUnderA m => forall eff. Transition dev proto l eff f a m (f a)) ->
    Transition dev proto l eff f a m (f a)
sharedWaiter runing waiter = do
    localChan <- asks stateVar_ >>= listenTSigVarIO
    bracket
        (liftIO $ atomically $ do
            v <- readTVar runing
            if not v
                then writeTVar runing True >> return Nothing
                else readTChan localChan >>= maybe retry (return . Just))

        (maybe (liftIO $ atomically $ writeTVar runing False) (const $ return ()))

        (maybe waiter return)

beginTransition :: (Eq (f a), Eq a, MonadUnderA m) => a -> Transition dev proto l eff f a m ()
beginTransition v = do
    var <- asks stateVar_
    t   <- asks transitionTarget_
    liftIO $ atomically $ do
        writeTSigVar var Nothing
        writeTSigVar t (Just v)

commitState :: (Eq (f a), MonadUnderA m) => f a -> Transition dev proto l eff f a m ()
commitState st = do
    var <- asks stateVar_
    liftIO $ atomically $ writeTSigVar var (Just st)

writeStateM :: MonadUnderA m => StateM dev proto l f a -> a -> Action dev proto (Impure eff) m ()
writeStateM stm v = withStateM stm $ transitionStart_ stm v

writeStateM' :: (Result f, Eq (f a), Eq a, MonadUnderA m) => StateM dev proto l f a -> a -> Action dev proto (Impure eff) m ()
writeStateM' stm v = withStateM stm $ do
    transitionStart_ stm v
    void $ runMaybeT $ (do
        res <- MaybeT $ tryResult <$> waitState_ stm
        t   <- MaybeT getTarget
        guard (res == t)) <|> throwM AbsurdRegisterWait
