{-# LANGUAGE DataKinds #-}

module Data.Property
(
    -- * Exceptions
    threeWayBracket,
    WhyNotStart(..),
    OutScopeTracking(..),
    TransitionFail(..),

    -- * Property API
    Property,
    PropertyConf(..),
    PropertyMeta(..),
    mkMeta,
    defaultPropertyConf,
    newProperty,
    cacheState,
    cacheInvalidateSTM,
    transitionState,
    requestStatus,
    askStatus,
    propertyName,
    propertyValidator,
    propertyEq,
    propertyAwait,
    propertyMutator,
    mkTransition,
    writeProperty,
    readProperty,
    trackProperty,
    subscribeStatus,

    -- * TransitionId API
    TransitionId,
    transitionTo,
    transitionStatusEvent,
    transitionStarted,
    transitionCompleted,
    transitionStartAwait,
    transitionEndAwait,
    subscribeTransition,

    -- * State API
    TransitionState(..),
    StateMap(..),
    completeState,
    progressState,
    localDrivenState,
    resultState,
    failState,
    otherLocalDrivenState
)
where

-- base
import Control.Applicative
import Control.Exception ( assert, throw )
import Control.Monad

import Data.Either
import Data.Function
import Data.Functor.Identity
import Data.Maybe
import Data.Typeable

-- async
import Control.Concurrent.Async

-- exceptions
import Control.Monad.Catch

-- mtl
import Control.Monad.Except

-- stm
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Monad.STM

-- transformers
import Control.Monad.Trans.Maybe

-- internal
--import Control.Concurrent.Utils
import Control.Monad.Action
import Control.Monad.Regions

data TransitionFail
    = MissTarget  -- ^ Transition end state not equal transition target
    | TimeoutFail
    | FailState
    | StuckState
    deriving (Eq, Show, Typeable)

instance Exception TransitionFail

data TransitionId dev s f a = TransitionId
    { transitionValueStream :: TChan (f a)
    , transitionTo          :: Maybe a
    , transitionStatus      :: TVar (Either (f a, STM Bool) (Maybe (f a, Maybe TransitionFail)))
    , transitionFinalizer   :: TVar (Maybe (TransitionFinalizer dev s)) }
    deriving (Eq, Typeable)

data TransitionFinalizer dev s = TransitionFinalizer { runTransitionFinalizer :: forall m. (MonadUnderA (Protocol dev m), MonadUnderA m) => Action dev Impure s m () }

transitionStatusEvent :: TransitionId dev s f a -> STM (Either (f a, STM Bool) (Maybe (f a, Maybe TransitionFail)))
transitionStatusEvent = readTVar . transitionStatus

data PropertyConf dev s f a = PropertyConf
    { mkPropertyGetter            :: forall eff m. (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m (f a)
    , mkPropertyWaiter            :: forall eff m. (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m (f a)
    , mkPropertyMutator           :: forall inner m. (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> a -> Action dev Impure s m ()
    , mkPropertyMissTargetHandler :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    , mkPropertyTimeoutHandler    :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    , mkPropertyFailStateHandler  :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    }

defaultPropertyConf :: PropertyConf dev s f a
defaultPropertyConf = PropertyConf
    { mkPropertyGetter            = undefined
    , mkPropertyMutator           = undefined
    , mkPropertyWaiter            = undefined
    , mkPropertyMissTargetHandler = const $ return ()
    , mkPropertyTimeoutHandler    = const $ return ()
    , mkPropertyFailStateHandler  = const $ return () }

data PropertyMeta a = PropertyMeta
    { mkPropertyName      :: String
    , mkPropertyEq        :: a -> a -> Bool
    , mkPropertyValidator :: a -> Either String a }

mkMeta :: Eq a => String -> PropertyMeta a
mkMeta name = PropertyMeta { mkPropertyName = name, mkPropertyEq = (==), mkPropertyValidator = return }

data Property dev s f a = Property
    { propertyName              :: String
    , propertyEq                :: a -> a -> Bool
    , propertyValidator         :: a -> Either String a

    , propertyCache             :: TVar (Maybe (f a))
    , propertyTransId           :: TVar (Maybe (TransitionId dev s f a))
    , propertyValueStream       :: TChan (f a)

    , requestStatus             :: forall m eff. (IfImpure ('R eff 'True), MonadAction dev m) => Action dev ('R eff 'True) s m (f a)
    , propertyAwait             :: forall m eff. (IfImpure ('R eff 'True), MonadAction dev m) => Action dev ('R eff 'True) s m (f a)
    , propertyMutator           :: forall m inner. (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => a -> Action dev Impure s m ()

    , propertyMissTargetHandler :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m ()
    , propertyTimeoutHandler    :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m ()
    , propertyFaileStateHandler :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m () }
    deriving Typeable

newProperty :: (StateMap f a, TransMult MonadIO (Protocol dev) m) => PropertyMeta a -> PropertyConf dev s f a -> Action dev Create s m (Property dev s f a)
newProperty meta conf = liftIO $ do
    cache     <- newTVarIO Nothing
    trId      <- newTVarIO Nothing
    getterVar <- newEmptyTMVarIO
    awaitVar  <- newEmptyTMVarIO
    stream    <- newBroadcastTChanIO
    let prop = Property
            { propertyName              = mkPropertyName meta
            , propertyEq                = mkPropertyEq meta
            , propertyValidator         = mkPropertyValidator meta
            , propertyCache             = cache
            , propertyTransId           = trId
            , propertyValueStream       = stream
            , requestStatus             = sharedEval (mkPropertyGetter conf prop >>= \v -> liftIO (atomically $ commitState prop v) >> return v) getterVar
            , propertyAwait             = sharedEval (mkPropertyWaiter conf prop >>= \v -> liftIO (atomically $ commitState prop v) >> return v) awaitVar
            , propertyMutator           = \target -> mkTransition prop (Just target) (mkPropertyMutator conf prop target) >>= either throwM return
            , propertyMissTargetHandler = mkPropertyMissTargetHandler conf prop
            , propertyTimeoutHandler    = mkPropertyTimeoutHandler conf prop
            , propertyFaileStateHandler = mkPropertyFailStateHandler conf prop }
    return prop



cacheState :: Property dev s f a -> STM (Maybe (f a))
cacheState = readTVar . propertyCache

transitionState :: Property dev s f a -> STM (Maybe (TransitionId dev s f a))
transitionState = readTVar . propertyTransId

transitionInvalidateSTM :: Property dev s f a -> STM ()
transitionInvalidateSTM prop = do
    mval  <- readTVar (propertyTransId prop)
    forM_ mval $ \_ ->
        writeTVar (propertyTransId prop) Nothing

-- | Discard current cache value. Probably safe operation.
cacheInvalidateSTM :: Property dev s f a -> STM ()
cacheInvalidateSTM prop = do
    mval <- readTVar (propertyCache prop)
    forM_ mval $ \_ ->
        writeTVar (propertyCache prop) Nothing


-- | Get property status from valid cache or request it from device
askStatus :: (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m (f a)
askStatus prop =
    liftIO (atomically $ cacheState prop) >>= maybe (requestStatus prop) return


commitState :: StateMap f a => Property dev s f a -> f a -> STM ()
commitState prop st = void $ runMaybeT $
    startingStateGuard <|> targetMissmatchCase <|> runningCase <|> otherCase <|> assert True undefined
    where
        -- If transition starting but not running skip all state equal old
        -- state.
        startingStateGuard = do
            transId                 <- MaybeT $ readTVar $ propertyTransId prop
            Left (from, timerEvent) <- lift $ readTVar $ transitionStatus transId
            guard (from == st)
            lift $ do
                expired <- timerEvent
                when expired $ do
                    cacheInvalidateSTM prop
--                    transitionInvalidateSTM prop
                    writeTVar (transitionStatus transId) (Right (Just (st, Just TimeoutFail)))


        -- If transition resulted but result not equal target state drop cache,
        -- target
        targetMissmatchCase = do
            result  <- MaybeT $ pure $ resultState st
            transId <- MaybeT $ readTVar $ propertyTransId prop
            target  <- MaybeT $ pure $ transitionTo transId
            guard $ not $ propertyEq prop result target
            lift $ do
                cacheInvalidateSTM prop
--                transitionInvalidateSTM prop
                writeTChan (transitionValueStream transId) st
                writeTChan (propertyValueStream prop) st
                writeTVar (transitionStatus transId) (Right (Just (st, Just MissTarget)))

        runningCase = do
            transId <- MaybeT $ readTVar $ propertyTransId prop
            lift $ do
                status <- readTVar $ transitionStatus transId
                when (isLeft status) $
                    writeTVar (transitionStatus transId) (Right Nothing)
                writeTChan (propertyValueStream prop) st
                writeTChan (transitionValueStream transId) st
                when cacheReason $ do
                    writeTVar (propertyCache prop) (Just st)
                    case completeState st of
                        Just _ -> do
                            transitionInvalidateSTM prop
                            writeTVar (transitionStatus transId) (Right (Just (st, Nothing)))
                        Nothing | failState st -> do
--                            transitionInvalidateSTM prop
                            writeTVar (transitionStatus transId) (Right (Just (st, Just FailState)))
                        _      -> return ()

        otherCase = lift $ do
            writeTChan (propertyValueStream prop) st
            when cacheReason $
                writeTVar (propertyCache prop) (Just st)
            unless (isJust (completeState st) || failState st) $ do
                var        <- newTVar $ Right Nothing
                tchan      <- newBroadcastTChan
                writeTVar (propertyTransId prop) $ Just TransitionId
                    { -- transitionKind          = Legacy
                    -- ,
                    transitionTo            = Nothing
                    , transitionValueStream   = tchan
                    , transitionStatus        = var
                    , transitionFinalizer     = undefined }

        cacheReason = localDrivenState st || failState st 
        {- || propertyToCache (propertyConf prop) st -}

instance Exception WhyNotStart

data WhyNotStart
    = ZeroMove
    | InappropriateStateForStart
    | ValidatorFail String
    deriving (Eq, Show, Typeable)

mkTransition :: (StateMap f a, MonadTrans (Protocol dev), MonadAction dev n, InScope m (Action dev Impure s n), MonadAction dev IO, MonadAction dev m)
    => Property dev s f a -- ^
    -> Maybe a -- ^ Optional transition target
    -> Action dev Impure s m b -- ^ Transition body
    -> Action dev Impure s m (Either WhyNotStart b)
mkTransition prop mtarget action = runExceptT $ do
    mtrid    <- liftIO $ readTVarIO $ propertyTransId prop
    forM_ mtrid $ \_ -> throwM InappropriateStateForStart
    start    <- lift $ askStatus prop
    mtarget' <- forM ((,) <$> mtarget <*> completeState start) $ \(target, old) -> do
        new  <- withExceptT ValidatorFail $ ExceptT $ return $ propertyValidator prop target
        when (propertyEq prop old new) $ throwM ZeroMove
        return new
    v        <- lift action
    lift $ mask_ $ do
        stream   <- liftIO newBroadcastTChanIO
        timer    <- liftIO $ newTVarIO False
        status   <- liftIO $ newTVarIO $ Left (start, readTVar timer)
        finalVar <- liftIO $ newTVarIO Nothing
        let transition = TransitionId
                { transitionTo          = mtarget'
                , transitionValueStream = stream
                , transitionStatus      = status
                , transitionFinalizer   = finalVar }
        liftIO $ atomically $ do
            cacheInvalidateSTM prop
            writeTVar (propertyTransId prop) $ Just transition
            writeTVar finalVar $ Just $ TransitionFinalizer $ do
                _ <- transitionEndAwait prop transition
                return ()
        onExit $ liftIO (readTVarIO finalVar) >>= mapM_ runTransitionFinalizer
        return v

-- | You try await new tracker-event out of tracker scope. Tracker can be
-- disable.
data OutScopeTracking = OutScopeTracking String
    deriving (Show, Typeable)

instance Exception OutScopeTracking

subscribeStatus :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m) => Property dev s f a -> (STM (f a) -> Action dev ('R eff 'True) s m b) -> Action dev ('R eff 'True) s m b
subscribeStatus prop action = bracket
    (do
        st   <- askStatus prop
        aval <- pureAsync $ forever $ propertyAwait prop
        chan <- liftIO $ atomically $ do
            chan <- dupTChan $ propertyValueStream prop
            unGetTChan chan st
            return chan
        return (aval, chan))

    (\(aval, chan) -> liftIO $ do
        cancel aval
        atomically $ unGetTChan chan $ throw $ OutScopeTracking $ propertyName prop)

    (\(_, chan) -> action (readTChan chan))


subscribeTransition :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev IO, MonadAction dev m) => Property dev s f a -> TransitionId dev s f a -> (STM (f a) -> Action dev ('R eff 'True) s m b) -> Action dev ('R eff 'True) s m b
subscribeTransition prop transition action = bracket
    (do
        st   <- askStatus prop
        aval <- pureAsync $ forever $ propertyAwait prop
        chan <- liftIO $ atomically $ do
            chan <- dupTChan $ transitionValueStream transition
            unGetTChan chan st
            return chan
        return (aval, chan))

    (\(aval, chan) -> liftIO $ do
        cancel aval
        atomically $ unGetTChan chan $ throw $ OutScopeTracking $ propertyName prop)

    (\(_, chan) -> action (readTChan chan))


transitionStarted :: TransitionId dev s f a -> STM (Maybe Bool)
transitionStarted transition = do
    status <- transitionStatusEvent transition
    case status of
        Left (old, timerEvent) -> do
            expired <- timerEvent
            if expired
                then do
                    writeTVar (transitionStatus transition) $ Right $ Just (old, Just TimeoutFail)
                    return $ Just False
                else return Nothing
        Right (Just (_, Just TimeoutFail)) -> return $ Just False
        Right _                            -> return $ Just True


transitionCompleted :: TransitionId dev s f a -> STM (Maybe (Maybe TransitionFail))
transitionCompleted transition = do
    status <- transitionStatusEvent transition
    case status of
        Left (old, timerEvent) -> do
            expired <- timerEvent
            if expired
                then do
                    writeTVar (transitionStatus transition) $ Right $ Just (old, Just TimeoutFail)
                    return $ Just $ Just TimeoutFail
                else return Nothing
        Right (Just (_, val))  -> return $ Just val
        _                      -> return Nothing


transitionStartAwait :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m) => Property dev s f a -> TransitionId dev s f a -> Action dev ('R eff 'True) s m Bool
transitionStartAwait prop transition = do
    mval <- liftIO $ atomically $ transitionStarted transition
    c    <- case mval of
        Just v  -> return v
        Nothing -> subscribeTransition prop transition $ \_ ->
            liftIO $ atomically $ transitionStarted transition >>= maybe retry return
    ifImpureM Ways
        { pureWay   = return ()
        , impureWay = unless c $ do
            clean <- liftIO $ readTVarIO $ transitionFinalizer transition
            unless (isNothing clean) $ do
                propertyTimeoutHandler prop
                liftIO $ atomically $ writeTVar (transitionFinalizer transition) Nothing }
    return c


transitionEndAwait :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m) => Property dev s f a -> TransitionId dev s f a -> Action dev ('R eff 'True) s m (Maybe TransitionFail)
transitionEndAwait prop transition = do
    mval <- liftIO $ atomically $ transitionCompleted transition
    merr <- case mval of
        Just v  -> return v
        Nothing -> subscribeTransition prop transition $ \stm -> do
            let conditionS = transitionCompleted transition >>= maybe retry return
                stuckCondition = fix $ \next -> do
                    val <- stm
                    if stateMap val == LocalDrivenIntermediate
                        then return $ Just FailState
                        else next
            condition <- ifImpure (conditionS <|> stuckCondition) conditionS
            liftIO $ atomically condition
    ifImpureM Ways
        { pureWay   = when (isNothing merr) $ liftIO $ atomically $ do
            mfinalizer <- readTVar $ transitionFinalizer transition
            forM_ mfinalizer $ \_ ->
                writeTVar (transitionFinalizer transition) Nothing
        , impureWay = do
            clean <- liftIO $ readTVarIO $ transitionFinalizer transition
            unless (isNothing clean) $ do
                case merr of
                    Just TimeoutFail -> propertyTimeoutHandler prop
                    Just FailState   -> propertyFaileStateHandler prop
                    Just MissTarget  -> propertyMissTargetHandler prop
                    _                -> return ()
                liftIO $ atomically $ writeTVar (transitionFinalizer transition) Nothing }
    return merr


-- | Wait current transition complete and start transition to new value
--
-- > writeProperty prop val1
-- > writeProperty prop val2
--
-- transite 'prop' first to 'val1', then to 'val2'.
writeProperty :: (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev IO, MonadAction dev m) => Property dev s f a -> a -> Action dev Impure s m ()
writeProperty prop val = do
    mtransition <- liftIO $ atomically $ transitionState prop
    forM_ mtransition $ \transition ->
        transitionEndAwait prop transition >>= mapM_ throwM
    propertyMutator prop val


-- | Wait current transition complete and return completeed value
--
-- > writeProperty prop val
-- > x <- readProperty prop
--
-- 'x' == 'val'
readProperty :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m, MonadAction dev IO) => Property dev s f a -> Action dev ('R eff 'True) s m a
readProperty prop = do
    res <- liftIO $ atomically $ (,) <$> transitionState prop <*> cacheState prop
    val <- case res of
        (Just transition, _) -> do
            transitionEndAwait prop transition >>= mapM_ throwM
            val <- liftIO $ atomically $ do
                Right (Just (val, Nothing)) <- transitionStatusEvent transition
                return val
            return val
        (_, Just val) -> return val
        _             -> askStatus prop
    maybe (throwM MissTarget) return (completeState val)


trackProperty :: (Typeable dev, MonadAction dev IO, MonadAction dev m, IfImpure ('R eff 'True), StateMap f b) => Property dev s f b -> (f b -> Action dev ('R eff 'True) s m ()) -> Action dev ('R eff 'True) s m ()
trackProperty prop act = do
    stat <- askStatus prop
    act stat
    unless (stopCondition stat) $
        subscribeStatus prop $ \event -> flip fix stat $ \again old -> do
            val <- liftIO $ atomically event
            unless (val == old) $ act val
            unless (stopCondition val) $ again val
    where
        stopCondition = not . progressState


threeWayBracket :: MonadMask m
    => m a -- ^ Safe resource allocation
    -> (a -> m err) -- ^ Safe resource error handle
    -> (a -> b -> m fin) -- ^ Safe resource normal handle
    -> (a -> m b) -- ^ Unmasked work with resource
    -> m b
threeWayBracket alloc err final eval = mask $ \unmask -> do
    res <- alloc
    val <- unmask (eval res) `onException` err res
    final res val
    return val


sharedEval :: (MonadIO m, MonadMask m) => m a -> TMVar (TMVar (Maybe a)) -> m a
sharedEval eval lock = threeWayBracket
    (liftIO $ atomically $ do
        mval          <- tryTakeTMVar lock
        (master, var) <- case mval of
            Nothing -> do
                var   <- newEmptyTMVar
                return (True, var)
            Just var -> return (False, var)
        putTMVar lock var
        return (master, var))

    (\(master, var) -> when master $ liftIO $ atomically $ do
        Just _ <- tryTakeTMVar lock
        True   <- tryPutTMVar var Nothing
        return ())

    (\(master, var) v -> when master $ liftIO $ atomically $ do
        Just _ <- tryTakeTMVar lock
        True   <- tryPutTMVar var (Just v)
        return ())

    (\(master, var) -> if master
        then eval
        else do
            mv <- liftIO $ atomically $ takeTMVar var
            maybe (sharedEval eval lock) return mv)


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
