module Data.Property
(
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
    PropertyEvent(..),
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
    -- * Events
    cacheState,
    transitionIdChangeEvent,
    subscribeReturn,
    nextReturn,
    subscribeValue,
    subscribeState,
    subscribeEvent,
    -- * Simple property access
    readProperty,
    writeProperty,
    trackProperty,
    -- * Transition access
    transiteTo,
    currentTransitionTarget,
    askStatus,
    requestStatus,
    commitState,
    cacheInvalidate,
    -- * Exceptions
    OutScopeTracking(..),
    TrackerKilled(..),
    WhyNotStart(..)
)
where

-- base
import Control.Applicative
import Control.Exception    ( assert )
import Control.Monad

-- import Data.Functor
import Data.Either
import Data.Functor.Identity
import Data.Typeable
import Data.Maybe

-- stm
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

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


-- | You try await new tracker-event out of tracker scope. Tracker can be
-- disable.
data OutScopeTracking = OutScopeTracking TypeRep String
    deriving (Show, Typeable)

data TrackerKilled = TrackerKilled TypeRep String
    deriving (Show, Typeable)

instance Exception OutScopeTracking
instance Exception TrackerKilled

data TransitionId f a = TransitionId
    { transitionTo          :: !a
    , transitionValueStream :: !(TChan (f a))
    , transitionStatus      :: !(TVar (Either a (Maybe (f a, Bool)))) }
    deriving (Eq, Typeable)

data Property dev s f a = Property
    { propertyConf         :: PropertyConf dev s f a
    , propertyCache        :: TVar (Maybe (f a))
    , propertyTransId      :: TVar (Maybe (TransitionId f a))
    , propertyEventStream  :: TChan (PropertyEvent f a)
    , propertyValueStream  :: TChan (f a)
    , propertySubscribes   :: TVar Integer
    , propertyTrackerState :: TVar (Maybe (Either SomeException ())) }
    deriving Typeable


data PropertyMeta a = PropertyMeta
    { propertyName         :: String               -- ^ Name
    , propertyStartTimeout :: Int                  -- ^ 
    , propertyValid        :: a -> Either String a -- ^ Validator/corrector
    , propertyEq           :: a -> a -> Bool       -- ^ Compare complete state
    } deriving Typeable


-- | Default Property metadata.
pMeta :: Eq a => PropertyMeta a
pMeta = PropertyMeta
    { propertyName = ""
    , propertyStartTimeout = -1
    , propertyValid = return
    , propertyEq = (==) }


-- | Property metadata accessor.
getPropertyMeta :: Property dev s f a -> PropertyMeta a
getPropertyMeta = propertyMeta . propertyConf


getPropertyDevTypeRep :: forall dev s f a. Typeable dev => Property dev s f a -> TypeRep
getPropertyDevTypeRep _ = typeOf (Proxy :: Proxy dev)

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


-- | Run custom transition with selected Property
selectProperty :: Monad m =>
    PropertySelector dev f a ->
    (forall s. Transition dev eff s f a m b) ->
    Action dev eff m b
selectProperty (PSel selector) act = withDevice $ \dev ->  withProperty (selector dev) act


withTracker :: (Typeable dev, MonadUnderA m) => Property dev s f a -> (STM () -> m b) -> m b
withTracker prop callback = bracket
    (liftIO $ atomically $ do
        modifyTVar (propertySubscribes prop) (+1)
        newTVar True)

    (\lock -> liftIO $ atomically $ do
        modifyTVar (propertySubscribes prop) (\x -> x - 1)
        writeTVar lock False)

    (\lock -> callback $ do
        c <- readTVar lock
        unless c $ throwM $ OutScopeTracking (getPropertyDevTypeRep prop) (propertyName $ getPropertyMeta prop))


cacheState :: Property dev s f a -> STM (Maybe (f a))
cacheState = readTVar . propertyCache


transitionIdEvent :: Property dev s f a -> STM (Maybe (TransitionId f a))
transitionIdEvent = readTVar . propertyTransId


transitionIdChangeEvent :: Eq a => Property dev s f a -> STM (STM Bool)
transitionIdChangeEvent prop = do
    transId <- transitionIdEvent prop
    return $ do
        transId' <- transitionIdEvent prop
        return (transId /= transId')

subscribeReturn :: (Typeable dev, IfImpure eff, StateMap f a, MonadUnderA m) => Property dev s f a -> (STM (f a) -> Action dev eff m b) -> Action dev eff m b
subscribeReturn prop callback = subscribeState prop $ \event ->
    callback $ fix $ \next -> do
        val <- event
        if progressState val
            then next
            else return val


nextReturn :: (Typeable dev, StateMap f a, IfImpure eff, MonadUnderA m) => Property dev s f a -> Action dev eff m (f a)
nextReturn prop = subscribeReturn prop (liftIO . atomically)


subscribeValue :: (IfImpure eff, Typeable dev, StateMap f a, MonadUnderA m) => Property dev s f a -> (STM a -> Action dev eff m b) -> Action dev eff m b
subscribeValue prop callback = subscribeState prop $ \event -> do
    stuck   <- transitionStuckCondition
    callback $ fix $ \next -> do
        val <- event
        case completeState val of
            Just v                     -> return v
            Nothing
                | stuck (stateMap val) -> assert False undefined
                | otherwise            -> next


subscribeEvent :: (StateMap f a, IfImpure eff, Typeable dev, MonadUnderA m) => Property dev s f a -> (STM (PropertyEvent f a) -> Action dev eff m b) -> Action dev eff m b
subscribeEvent prop callback = withTracker prop $ \outScopeCheck -> do
    stat  <- withProperty prop askStatus
    event <- liftIO $ atomically $ do
        mstat <- cacheState prop
        tchan <- dupTChan $ propertyEventStream prop
        maybe (unGetTChan tchan (StateE stat)) (\stat' -> unGetTChan tchan (StateE stat') >> unGetTChan tchan (CacheE True)) mstat
        return tchan
    callback $ do
        outScopeCheck
        readTChan event <|> notTrack
    where
        notTrack = do
            status <- readTVar $ propertyTrackerState prop
            guard $ isJust status
            throwM $ TrackerKilled (getPropertyDevTypeRep prop) (propertyName $ getPropertyMeta prop)


subscribeState :: (StateMap f a, IfImpure eff, Typeable dev, MonadUnderA m) => Property dev s f a -> (STM (f a) -> Action dev eff m b) -> Action dev eff m b
subscribeState prop callback = subscribeEvent prop $ \event ->
    callback $ fix $ \again -> event >>= maybe again return . stateEvent


-- | Wait current transition complete and start transition to new value
--
-- > writeProperty prop val1
-- > writeProperty prop val2
--
-- transite 'prop' first to 'val1', then to 'val2'.
writeProperty :: (Typeable dev, MonadUnderA m, StateMap f a) => Property dev s f a -> a -> Action dev (Impure eff) m ()
writeProperty prop val = withProperty prop $ do
    -- Wait recent transition end
    readProperty prop
    ereason <- transiteTo val
    case ereason of
        Left InappropriateStateForStart -> error "Inappropriate state for start"
        Left (StartingTimeoutFail t)    -> error ("Transition starting too long: over " ++ show t ++ "us")
        Left (ValidatorFail str)        -> error str
        _                               -> return ()


-- | Wait current transition complete and return completeed value
--
-- > writeProperty prop val
-- > x <- readProperty prop
--
-- 'x' == 'val1'
readProperty :: (Typeable dev, MonadUnderA m, IfImpure eff, StateMap f b) => Property dev s f b -> Action dev eff m b
readProperty prop = do
    tridChngEvnt <- liftIO $ atomically $ transitionIdChangeEvent prop
    subscribeValue prop $ \ev -> liftIO $ atomically (ev <|> (tridChngEvnt >>= guard >> assert False (error "Пыщ!")))


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


data WhyNotStart
    = ZeroMove
    | InappropriateStateForStart
    | ValidatorFail String
    | StartingTimeoutFail Int
    deriving (Eq, Ord, Show, Typeable)


transiteTo :: (Typeable dev, StateMap f a, MonadUnderA m) => a -> Transition dev (Impure eff) s f a m (Either WhyNotStart a)
transiteTo target = runExceptT $ do
    prop     <- ask
    cTarget  <- ExceptT $ return $ either (Left . ValidatorFail) Right $ propertyValid (getPropertyMeta prop) target
    oldState <- ExceptT $ maybe (Left InappropriateStateForStart) Right . completeState <$> askStatus
    when (propertyEq (getPropertyMeta prop) oldState cTarget) $ throwE ZeroMove
    ExceptT $ bracketOnError
        (liftIO $ do
            atomically $ do
                cacheInvalidateSTM prop
                var1  <- newTVar $ Left oldState
                tchan <- newBroadcastTChan
                writeTVar (propertyTransId prop) $ Just TransitionId
                    { transitionTo            = cTarget
                    , transitionValueStream   = tchan
                    , transitionStatus        = var1 }
                writeTChan (propertyEventStream prop) (TargetE (Just cTarget))
            if propertyStartTimeout (getPropertyMeta prop) < 0
                then newTVarIO False
                else registerDelay (propertyStartTimeout $ propertyMeta $ propertyConf prop))

        (const $ liftIO $ atomically $ transitionInvalidateSTM prop)

        (\timer -> do
            propertyMutator (propertyConf prop) cTarget
            subscribeState prop $ \event -> liftIO $ do
                mval <- atomically ((Just <$> event) <|> (readTVar timer >>= guard >> return Nothing))
                case mval of
                    Nothing -> do
                        atomically $ transitionInvalidateSTM prop
                        return $ Left $ StartingTimeoutFail $ propertyStartTimeout $ getPropertyMeta prop

                    _       -> return $ Right cTarget)


transitionStuckCondition :: (MonadUnderA m, StateMap f a, IfImpure eff) => Action dev eff m (f a -> Bool)
transitionStuckCondition = ifImpure impureCondition pureCondition
    where
        impureCondition st = stateMap st == Fail || stateMap st == LocalDrivenIntermediate

        pureCondition st   = stateMap st == Fail


transitionTargetEvent :: MonadUnderA m => Transition dev eff s f a m (STM (Maybe a))
transitionTargetEvent = asks $ \prop -> runMaybeT $ do
    trId <- MaybeT $ readTVar (propertyTransId prop)
    return $ transitionTo trId


currentTransitionTarget :: MonadUnderA m => Transition dev eff s f a m (Maybe a)
currentTransitionTarget = transitionTargetEvent >>= liftIO . atomically


-- | Request property stataus from device
requestStatus :: (StateMap f a, IfImpure eff, MonadUnderA m) => Transition dev eff s f a m (f a)
requestStatus = ask >>= \prop -> do
    st <- propertyGetter $ propertyConf prop
    commitState st
    return st


-- | Get property status from valid cache or request it from device
askStatus :: (StateMap f a, IfImpure eff, MonadUnderA m) => Transition dev eff s f a m (f a)
askStatus = ask >>= \prop ->
    liftIO (atomically $ readTVar $ propertyCache prop) >>= maybe requestStatus return


commitStateSTM :: StateMap f a => Property dev s f a -> f a -> STM ()
commitStateSTM prop val = void $ runMaybeT $ do
    startingStateGuard <|> targetMissmatchCase <|> otherCase
    where
        -- If transition starting but not running skip all state equal old
        -- state.
        startingStateGuard  = do
            result  <- MaybeT $ pure $ resultState val
            transId <- MaybeT $ readTVar $ propertyTransId prop
            status  <- lift $ readTVar $ transitionStatus transId
            case status of
                Left from | not $ propertyEq (getPropertyMeta prop) from result -> return ()
                _                                                               -> empty

        -- If transition resulted but result not equal target state drop cache,
        -- target and generate MissTargetE
        targetMissmatchCase = do
            result  <- MaybeT $ pure $ resultState val
            transId <- MaybeT $ readTVar $ propertyTransId prop
            guard $ not $ propertyEq (getPropertyMeta prop) result (transitionTo transId)
            lift $ do
                cacheInvalidateSTM prop
                transitionInvalidateSTM prop
                writeTChan (propertyEventStream prop) MissTargetE
                writeTChan (transitionValueStream transId) val
                writeTVar (transitionStatus transId) (Right (Just (val, False)))

        otherCase = do
            transId    <- MaybeT (readTVar $ propertyTransId prop) <|> assert False undefined
            lift $ do
                status <- readTVar $ transitionStatus transId
                when (isLeft status) $ writeTVar (transitionStatus transId) (Right Nothing)
                writeTChan (propertyEventStream prop) (StateE val)
                when (localDrivenState val || failState val || propertyToCache (propertyConf prop) val) $ do
                    writeTVar (propertyCache prop) (Just val)
                    writeTChan (propertyEventStream prop) (CacheE True)
                    writeTChan (transitionValueStream transId) val
                forM_ (completeState val) $ \_ -> do
                    transitionInvalidateSTM prop
                    writeTVar (transitionStatus transId) (Right (Just (val, True)))



transitionInvalidateSTM :: Property dev s f a -> STM ()
transitionInvalidateSTM prop = do
    mval  <- readTVar (propertyTransId prop)
    forM_ mval $ \_ -> do
        writeTVar (propertyTransId prop) Nothing
        writeTChan (propertyEventStream prop) (TargetE Nothing)


-- | Discard current cache value. Probably safe operation.
cacheInvalidateSTM :: Property dev s f a -> STM ()
cacheInvalidateSTM prop = do
    mval <- readTVar (propertyCache prop)
    forM_ mval $ \_ -> do
        writeTVar (propertyCache prop) Nothing
        writeTChan (propertyEventStream prop) (CacheE False)


commitState :: (StateMap f a, MonadUnderA m) => f a -> Transition dev eff s f a m ()
commitState val = do
    prop <- ask
    liftIO $ atomically $ commitStateSTM prop val


-- | Discard current cache value. Probably safe operation.
cacheInvalidate :: MonadUnderA m => Transition dev eff s f a m ()
cacheInvalidate = ask >>= liftIO . atomically . cacheInvalidateSTM


newProperty
    :: (Typeable dev, Typeable f, Typeable a, MonadUnderA m, MonadAccum (CreateCtx d (Action dev Create IO)) m, StateMap f a)
    => Action (New dev) Create m (PropertyConf dev () f a, Maybe (f a)) -- ^
    -> Action (New dev) Create m (Property dev () f a)
newProperty constr = do
    cacheVar     <- liftIO $ newTVarIO Nothing
    transIdVar   <- liftIO $ newTVarIO Nothing
    eventStream  <- liftIO newBroadcastTChanIO
    subs         <- liftIO $ newTVarIO 0
    resTrack     <- liftIO $ newTVarIO Nothing
    (conf, mval) <- constr

    let property = Property
            { propertyConf         = conf
                { propertyMeta = (propertyMeta conf)
                    { propertyName =
                        if null (propertyName (propertyMeta conf))
                            then show (typeOf property)
                            else propertyName (propertyMeta conf)
                    }
                }
            , propertyCache        = cacheVar
            , propertyTransId      = transIdVar
            , propertyEventStream  = eventStream
            , propertySubscribes   = subs
            , propertyTrackerState = resTrack }

    forM_ mval $ \_ -> liftIO $ atomically $ writeTVar cacheVar mval

    forM_ (propertyTracker conf) $ \tracker ->
        addToStage $ void $ pureAsync $ try (withProperty property $
            tracker ((> 0) <$> readTVar subs) commitState) >>= liftIO . atomically . writeTVar resTrack . Just

    return property

data PropertyEvent f a
    = StateE  !(f a)     -- ^ New state
    | CacheE  !Bool      -- ^ Switch cache state
    | TargetE !(Maybe a) -- ^ Switch transition target
    | MissTargetE        -- ^ Transition target missmatch
    deriving (Eq, Ord, Show, Read, Typeable)


-- | Any event contain state
stateEvent :: StateMap f a => PropertyEvent f a -> Maybe (f a)
stateEvent pev = case pev of
    StateE st -> pure st
    _         -> Nothing


data TransitionState a
    = Final a
    | OtherComplete a
    | LocalDrivenIntermediate
    | RemoteDriven
    | Fail
    deriving (Eq, Ord, Show, Read, Typeable)

class (Typeable a, Typeable f, Eq a, Eq (f a)) => StateMap f a where
    stateMap :: f a -> TransitionState a

instance (Typeable s, Eq s) => StateMap TransitionState s where
    stateMap = id

instance (Typeable a, Eq a) => StateMap Identity a where
    stateMap = Final . runIdentity

instance (Typeable a, Eq a) => StateMap Maybe a where
    stateMap = maybe RemoteDriven Final

instance (Typeable a, Typeable b, Eq a, Eq b) => StateMap (Either b) a where
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
