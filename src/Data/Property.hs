module Data.Property (
    -- * Types
    Transition,
    Property,
    PropertySelector(..),
    withProperty,
    selectProperty,
    PropertyMeta(..),
    pMeta,
    getPropertyMeta,
    PropertyConf(..),
    TransitionState(..),
    StateMap(..),
    failState,
    resultState,
    otherLocalDrivenState,
    completeState,
    progressState,
    localDrivenState,
    -- * Create property
    newProperty,
    -- newLocalCache,
    -- * Events
    cacheState,
    subscribeReturn,
    nextReturn,
    subscribeValue,
    nextValue,
    subscribeState,
    -- * Simple property access
    readProperty,
    writeProperty,
    trackProperty,
    -- * Transition access
    transiteTo,
    waitTransition,
    currentTransitionTarget,
    askStatus,
    requestStatus,
    commitState,
    cacheInvalidate,
    -- * Exceptions
    OutScopeTracking(..),
    WhyNotStart(..)
) where

-- base
import Control.Applicative
import Control.Monad

import Data.Functor.Identity
import Data.Typeable
import Data.Maybe

-- stm
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

-- mtl
import Control.Monad.Reader
-- import Control.Monad.Writer

-- exceptions
import Control.Monad.Catch

-- internal
import Control.Monad.Accum
import Control.Monad.Action
import Control.Monad.Action.Internal

type Transition dev eff s f b m a = Action dev eff (ReaderT (Property dev s f b) m) a


data OutScopeTracking = OutScopeTracking TypeRep String
    deriving (Show, Typeable)

instance Exception OutScopeTracking


data Property dev s f a = Property
    { propertyConf         :: PropertyConf dev s f a
    , propertyCache        :: TVar (Maybe (f a))
    , propertyTransTarget  :: TVar (Maybe a)
    , propertyStream       :: TChan (f a)
    , propertySubscribes   :: TVar Integer
    , propertyTrackerState :: TVar (Maybe (Either SomeException ())) }
    deriving Typeable


data PropertyMeta a = PropertyMeta
    { propertyName  :: String               -- ^ Name
    , propertyValid :: a -> Either String a -- ^ Validator/corrector
    , propertyEq    :: a -> a -> Bool       -- ^ Compare complete state
    } deriving Typeable

-- | Default Property metadata.
pMeta :: Eq a => PropertyMeta a
pMeta = PropertyMeta { propertyName = "", propertyValid = return, propertyEq = (==) }


-- | Property metadata accessor.
getPropertyMeta :: Property dev s f a -> PropertyMeta a
getPropertyMeta = propertyMeta . propertyConf


data PropertyConf dev s f a = PropertyConf
    { propertyMeta    :: PropertyMeta a
    , propertyToCache :: f a -> Bool
    , propertyGetter  :: forall m eff. (IfImpure eff, MonadUnderA m) => Transition dev eff s f a m (f a)
    , propertyTracker :: forall m. MonadUnderA m => Maybe (STM Bool -> (f a -> Transition dev Pure s f a m ()) -> Transition dev Pure s f a m ())
    -- | Start property mutation from to new 'resultState'.
    , propertyMutator :: forall m. MonadUnderA m => a -> forall eff. Transition dev (Impure eff) s f a m () }
    deriving Typeable

-- | Run custom transition with property
withProperty :: Monad m => Property dev s f a -> Transition dev eff s f a m b -> Action dev eff m b
withProperty p = unsafeMapAction (flip runReaderT p)


data PropertySelector dev f a = PSel (forall s. dev s -> Property dev s f a)

selectProperty :: Monad m =>
    PropertySelector dev f a ->
    (forall s. Transition dev eff s f a m b) ->
    Action dev eff m b
selectProperty (PSel selector) act = withDevice $ \dev ->  withProperty (selector dev) act


withTracker :: forall dev s f a m b. (Typeable dev, MonadUnderA m) => Property dev s f a -> (STM () -> m b) -> m b
withTracker prop callback = bracket
    (liftIO $ atomically $ do
        modifyTVar (propertySubscribes prop) (+1)
        newTVar True)

    (\lock -> liftIO $ atomically $ do
        modifyTVar (propertySubscribes prop) (\x -> x - 1)
        writeTVar lock False)

    (\lock ->
        let outScopeCheck = do
                c <- readTVar lock
                unless c $ throwM $ OutScopeTracking (typeRep (Proxy :: Proxy dev)) (propertyName $ propertyMeta $ propertyConf prop)

        in callback outScopeCheck)


cacheState :: Property dev s f a -> STM (Maybe (f a))
cacheState = readTVar . propertyCache


subscribeReturn :: (Typeable dev, StateMap f a, MonadUnderA m) => Property dev s f a -> (STM (f a) -> m b) -> m b
subscribeReturn prop callback = subscribeState prop $ \event ->
    callback $ fix $ \next -> do
        val <- event
        if progressState val
            then return val
            else next


nextReturn :: (Typeable dev, StateMap f a, MonadUnderA m) => Property dev s f a -> m (f a)
nextReturn prop = subscribeReturn prop (liftIO . atomically)


subscribeValue :: (IfImpure eff, Typeable dev, StateMap f a, MonadUnderA m) => Property dev s f a -> (STM a -> Action dev eff m b) -> Action dev eff m b
subscribeValue prop callback = subscribeState prop $ \event -> do
    stuck   <- transitionStuckCondition
    callback $ fix $ \next -> do
        val <- event
        case completeState val of
            Just v              -> return v
            Nothing
                | stuck (stateMap val)      -> _
                | otherwise                 -> next


nextValue :: (IfImpure eff, Typeable dev, StateMap f a, MonadUnderA m) => Property dev s f a -> Action dev eff m a
nextValue prop = subscribeValue prop (liftIO . atomically)


subscribeState :: (Typeable dev, MonadUnderA m) => Property dev s f a -> (STM (f a) -> m b) -> m b
subscribeState prop callback = withTracker prop $ \outScopeCheck -> do
    tchan <- liftIO $ atomically $ dupTChan $ propertyStream prop

    let notTrack = do
            status <- readTVar $ propertyTrackerState prop
            case status of
                Nothing -> empty
                Just _  -> throwM (_ :: SomeException)

    callback $ outScopeCheck >> readTChan tchan <|> notTrack


writeProperty :: (Typeable dev, MonadUnderA m, StateMap f a) => Property dev s f a -> a -> Action dev (Impure eff) m ()
writeProperty prop val = withProperty prop $ do
    ereason <- transiteTo val
    case ereason of
        Left ZeroMove                   -> return ()
        Left InappropriateStateForStart -> error "Inappropriate state for start"
        Left (ValidatorFail str)        -> error str
        Right _                         -> waitTransition


readProperty :: (Typeable dev, MonadUnderA m, IfImpure eff, StateMap f b) => Property dev s f b -> Action dev eff m b
readProperty prop = withProperty prop $ subscribeValue prop $ \event -> do
    stat <- askStatus
    case completeState stat of
        Just v               -> return v
        Nothing
            | failState stat -> _
            | otherwise      -> liftIO (atomically event)


trackProperty :: (Typeable dev, MonadUnderA m, IfImpure eff, StateMap f b) => Property dev s f b -> (f b -> Action dev eff m ()) -> Action dev eff m ()
trackProperty prop act = do
    stat <- withProperty prop askStatus
    act stat
    unless (stopCondition stat) $ subscribeState prop $ \event -> flip fix stat $ \again old -> do
        val <- liftIO $ atomically event
        unless (val == old) $ act val
        unless (stopCondition val) $ again val
    where
        stopCondition = not . progressState


data WhyNotStart = ZeroMove | InappropriateStateForStart | ValidatorFail String
    deriving (Eq, Ord, Show, Typeable)


transiteTo :: (Typeable dev, StateMap f a, MonadUnderA m) => a -> Transition dev (Impure eff) s f a m (Either WhyNotStart a)
transiteTo target = runExceptT $ do
    prop     <- ask
    cTarget  <- ExceptT $ return $ either (Left . ValidatorFail) Right $ propertyValid (getPropertyMeta prop) target
    oldState <- ExceptT $ maybe (Left InappropriateStateForStart) Right . completeState <$> askStatus
    when (propertyEq (getPropertyMeta prop) oldState cTarget) $ throwE ZeroMove
    lift $ do
        cacheInvalidate
        liftIO $ atomically $ writeTVar (propertyTransTarget prop) (Just cTarget)
        propertyMutator (propertyConf prop) cTarget
        liftIO $ subscribeState prop $ \event -> fix $ \next -> do
            val <- atomically event
            forM_ (resultState val) $ \cState -> when (propertyEq (getPropertyMeta prop) oldState cState) next
    return cTarget


transitionStuckCondition :: (MonadUnderA m, StateMap f a, IfImpure eff) => Transition dev eff s f a m (TransitionState a -> Bool)
transitionStuckCondition = ifImpure impureCondition pureCondition
    where
        impureCondition st = case stateMap st of
            Fail                    -> True
            LocalDrivenIntermediate -> True
            _                       -> False

        pureCondition st = stateMap st == Fail


waitTransition :: (Typeable dev, IfImpure eff, StateMap f a, MonadUnderA m) => Transition dev eff s f a m ()
waitTransition = do
    prop           <- ask
    transSt        <- stateMap <$> askStatus
    mtarget        <- currentTransitionTarget
    stuckCondition <- transitionStuckCondition
    let condition = propertyEq (getPropertyMeta prop)
    case transSt of
        _ | stuckCondition transSt  -> _
        Final v | Just t <- mtarget -> unless (condition t v) _
        RemoteDriven                -> do
            pos <- nextValue prop
            forM_ mtarget $ \target -> unless (condition target pos) _
        LocalDrivenIntermediate     -> do
            pos <- nextValue prop
            forM_ mtarget $ \target -> unless (condition target pos) _
        _                           -> return ()

currentTransitionTarget :: MonadUnderA m => Transition dev eff s f a m (Maybe a)
currentTransitionTarget = ask >>= liftIO . atomically . readTVar . propertyTransTarget


requestStatus :: (StateMap f a, IfImpure eff, MonadUnderA m) => Transition dev eff s f a m (f a)
requestStatus = ask >>= \prop -> do
    st <- propertyGetter $ propertyConf prop
    commitState st
    return st


askStatus :: (StateMap f a, IfImpure eff, MonadUnderA m) => Transition dev eff s f a m (f a)
askStatus = ask >>= \prop ->
    liftIO (atomically $ readTVar $ propertyCache prop) >>= maybe requestStatus return


commitState :: (StateMap f a, MonadUnderA m) => f a -> Transition dev eff s f a m ()
commitState val = do
    prop <- ask
    liftIO $ atomically $ writeTChan (propertyStream prop) val
    runMaybeT $ do
        result <- MaybeT $ pure $ resultState val
        target <- MaybeT $ liftIO $ atomically $ swapTVar (propertyTransTarget prop) Nothing
        guard (not $ propertyEq (getPropertyMeta prop) result target)
        lift cacheInvalidate
        liftIO $ atomically $ writeTChan (propertyStream prop) (error "target not equal destination")
    when (localDrivenState val || propertyToCache (propertyConf prop) val)
        (liftIO $ atomically $ writeTVar (propertyCache prop) (Just val))


cacheInvalidate :: MonadUnderA m => Transition dev eff s f a m ()
cacheInvalidate = ask >>= \prop -> liftIO $ atomically $ do
    mval <- readTVar (propertyCache prop)
    when (isJust mval) $ writeTVar (propertyCache prop) Nothing


newProperty
    :: (MonadUnderA m, MonadAccum (CreateCtx d (Action dev Create IO)) m, StateMap f a)
    => Action (New dev) Create m (PropertyConf dev s f a, Maybe (f a)) -- ^
    -> Action (New dev) Create m (Property dev s f a)
newProperty constr = do
    cacheVar     <- liftIO $ newTVarIO Nothing
    targetVar    <- liftIO $ newTVarIO Nothing
    stream       <- liftIO newBroadcastTChanIO
    subs         <- liftIO $ newTVarIO 0
    resTrack     <- liftIO $ newTVarIO Nothing
    (conf, mval) <- constr

    let property = Property
            { propertyConf         = conf
            , propertyCache        = cacheVar
            , propertyTransTarget  = targetVar
            , propertyStream       = stream
            , propertySubscribes   = subs
            , propertyTrackerState = resTrack }

    forM_ mval $ liftIO . atomically . writeTVar cacheVar . Just

    forM_ (propertyTracker conf) $ \tracker ->
        addToStage $ void $ pureAsync $ try (withProperty property $
            tracker ((> 0) <$> readTVar subs) commitState) >>= liftIO . atomically . writeTVar resTrack . Just

    return property


-- newLocalCache :: (Eq a, MonadUnderA m) =>
--     PropertyMeta a ->
--     a ->
--     (forall m1. MonadUnderA m1 => a -> forall eff. Action dev (Impure eff) m1 ()) ->
--     Action dev Create m (Property dev () Identity a)
-- newLocalCache metaData initValue mutator = newProperty $ do
--     mutator initValue
--     return (PropertyConf
--         { propertyMeta    = metaData
--         , propertyToCache = const False
--         , propertyTracker = Nothing
--         , propertyMutator = \v -> (mutator v >> commitState (pure v)) `finally` cacheInvalidate
--         , propertyGetter  = error "FUUUUUUUU!!!" }, Just (pure initValue))

data TransitionState a
    = Final a
    | OtherComplete a
    | LocalDrivenIntermediate
    | RemoteDriven
    | Fail
    deriving (Eq, Ord, Show, Read, Typeable)

class (Eq a, Eq (f a)) => StateMap f a where
    stateMap :: f a -> TransitionState a

instance Eq s => StateMap TransitionState s where
    stateMap = id

instance Eq a => StateMap Identity a where
    stateMap = Final . runIdentity

instance Eq a => StateMap Maybe a where
    stateMap = maybe RemoteDriven Final

instance (Eq a, Eq b) => StateMap (Either b) a where
    stateMap = either (const RemoteDriven) Final

-- | Any complete state
completeState :: StateMap f a => f a -> Maybe a
completeState val = case stateMap val of
    Final v         -> pure v
    OtherComplete v -> pure v
    _               -> Nothing

-- | Intermediate state with remote control
progressState :: StateMap f a => f a -> Bool
progressState val = stateMap val == RemoteDriven

-- | Any state with local control except 'failState'
localDrivenState :: StateMap f a => f a -> Bool
localDrivenState val = case stateMap val of
    Final _                 -> True
    OtherComplete _         -> True
    LocalDrivenIntermediate -> True
    _                       -> False

-- | Final transition state where state value equal transition target value
resultState :: StateMap f a => f a -> Maybe a
resultState val
    | Final v <- stateMap val = pure v
    | otherwise               = Nothing

-- | Target not reachable
failState :: StateMap f a => f a -> Bool
failState val = stateMap val == Fail

-- | Intermediate state with local control
otherLocalDrivenState :: StateMap f a => f a -> Bool
otherLocalDrivenState val = stateMap val == LocalDrivenIntermediate
