{-# LANGUAGE DataKinds #-}

module Data.Property
(
    -- * Exceptions
    threeWayBracket,
    sharedEval,
    WhyNotStart(..),
    OutScopeTracking(..),
    PropertyException(..),
    TransitionFail(..),

    -- * Property API
    Property,
    PropertyOptional(..),
    propertyOptional,
    newProperty,
    cacheState,
    cacheInvalidateSTM,
    transitionState,
    transitionInvalidateSTM,
    transitionForceInvalidateSTM,
    requestStatus,
    askStatus,
    propertyName,
    propertyValidator,
    propertyEq,
    propertyAwait,
    propertyMutator,
    propertyTransitionCheck,
    propertyTransitionCheck',
    throwPropertyException,
    mkTransition,
    mkUnsafeTransition,
    mkUnsafeTransition2,
    writeProperty,
    readProperty,
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

    -- * Debug API
    PropertyInfo(..),
    takePropertyInfo,
    takePropertyInfo',
    TransitionInfo(..),
    takeTransitionInfo,
    takeTransitionInfo',
    trackProperty,
    trackPropertyInfo,
    forceTransitionRelease,
    forceTransitionRelease',

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
import Control.Exception ( {- assert, -} throw )
import Control.Monad

import Data.Either
import Data.Function
import Data.Functor.Identity
import Data.Maybe
import Data.Typeable

import GHC.Stack

-- async
import Control.Concurrent.Async

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Monad.STM

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

-- internal
--import Control.Concurrent.Utils
import Control.Monad.Action
import Control.Monad.Regions

data PropertyException = forall f a e. (Show (f a), Show a, Show e) => PropertyException { broken :: e, brokenPropertyInfo :: PropertyInfo f a }

newtype WrappedState f a = WrappedState (f a)
    deriving (Eq, Show)

deriving instance Show PropertyException
deriving instance Typeable PropertyException

instance Exception PropertyException

data TransitionFail f a
    = MissTarget { targetPos :: Maybe a, finalState :: f a } -- ^ Transition end state not equal transition target
    | TimeoutFail
    | FailState (f a)
    | StuckState (f a)
    deriving (Eq, Show, Typeable)

data TransitionId dev s f a = TransitionId
    { transitionValueStream :: TChan (f a)
    , transitionTo          :: Maybe a
    , transitionStatus      :: TVar (Either (f a, STM Bool) (Maybe (f a, Maybe (TransitionFail f a))))
    , transitionFinalizer   :: TVar (Maybe (TransitionFinalizer dev s)) }
    deriving (Eq, Typeable)

data TransitionFinalizer dev s = TransitionFinalizer { runTransitionFinalizer :: forall m. (MonadUnderA (Protocol dev m), MonadUnderA m) => Action dev Impure s m () }

transitionStatusEvent :: TransitionId dev s f a -> STM (Either (f a, STM Bool) (Maybe (f a, Maybe (TransitionFail f a))))
transitionStatusEvent = readTVar . transitionStatus

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
    , propertyMutationPrecheck  :: forall m. MonadAction dev m => dev s -> [Bool -> Action dev Impure s m (Maybe String)]

    , propertyMissTargetHandler :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m ()
    , propertyTimeoutHandler    :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m ()
    , propertyFailStateHandler  :: forall m. (StateMap f a, MonadAction dev m) => Action dev Impure s m () }
    deriving Typeable

forceTransitionRelease' :: MonadIO m => Property dev s f a -> m (Maybe (TransitionId dev s f a))
forceTransitionRelease' = liftIO . mask_ . atomically . forceTransitionRelease

forceTransitionRelease :: Property dev s f a -> STM (Maybe (TransitionId dev s f a))
forceTransitionRelease prop = do
    mtransition <- readTVar $ propertyTransId prop
    forM mtransition $ \transition -> do
        cacheInvalidateSTM prop
        transitionInvalidateSTM prop
        return transition

data PropertyInfo f a = PropertyInfo
    { propertyName_        :: String
    , propertyCache_       :: Maybe (f a)
    , propertyTransition_  :: Maybe (TransitionInfo f a) }
    deriving (Eq, Show, Typeable)

takePropertyInfo :: Property dev s f a -> STM (PropertyInfo f a)
takePropertyInfo prop = PropertyInfo (propertyName prop) <$> readTVar (propertyCache prop) <*> (readTVar (propertyTransId prop) >>= mapM takeTransitionInfo)

takePropertyInfo' :: MonadIO m => Property dev s f a -> m (PropertyInfo f a)
takePropertyInfo' = liftIO . atomically . takePropertyInfo

data TransitionInfo f a = TransitionInfo
    { transitionTo_        :: Maybe a
    , transitionStatus_    :: Either (f a, Bool) (Maybe (f a, Maybe (TransitionFail f a)))
    , transitionCleanuped_ :: Bool }
    deriving (Eq, Show, Typeable)

takeTransitionInfo' :: MonadIO m => TransitionId dev s f a -> m (TransitionInfo f a)
takeTransitionInfo' = liftIO . atomically . takeTransitionInfo

takeTransitionInfo :: TransitionId dev s f a -> STM (TransitionInfo f a)
takeTransitionInfo transition = do
    status' <- readTVar (transitionStatus transition) >>= \status -> case status of
        Left (v, timerEvent) -> do
            expired <- timerEvent
            return $ Left (v, expired)
        Right v       -> return $ Right v
    mfinal  <- readTVar (transitionFinalizer transition)
    return TransitionInfo { transitionTo_ = transitionTo transition, transitionStatus_ = status', transitionCleanuped_ = isNothing mfinal }

data PropertyOptional dev s f a = PropertyOptional
    { mkPropertyEq                :: a -> a -> Bool
    , mkPropertyValidator         :: a -> Either String a
    , mkPropertyMutationPrecheck  :: forall m. MonadAction dev m => Property dev s f a -> dev s -> [Bool -> Action dev Impure s m (Maybe String)]
    , mkPropertyMissTargetHandler :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    , mkPropertyTimeoutHandler    :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    , mkPropertyFailStateHandler  :: forall m. (StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev Impure s m ()
    }

propertyOptional :: StateMap f a => PropertyOptional dev s f a
propertyOptional = PropertyOptional
    { mkPropertyEq                = (==)
    , mkPropertyValidator         = return
    , mkPropertyMutationPrecheck  = \_ _ -> []
    , mkPropertyMissTargetHandler = const $ return ()
    , mkPropertyTimeoutHandler    = const $ return ()
    , mkPropertyFailStateHandler  = const $ return () }

newProperty :: StateMap f a
    => String -- ^ Property name
    -> (forall m eff. (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m (f a))
    -> (forall m eff. (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m (f a))
    -> (forall m inner. (MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> a -> Action dev Impure s m ())
    -> PropertyOptional dev s f a
    -> IO (Property dev s f a)
newProperty propertyName' requestStatus' propertyAwait' propertyMutator' extraPart = do
    cache     <- newTVarIO Nothing
    trId      <- newTVarIO Nothing
    stream    <- newBroadcastTChanIO
    let prop = Property
            { propertyName              = propertyName'
            , propertyEq                = mkPropertyEq extraPart
            , propertyValidator         = mkPropertyValidator extraPart
            , propertyCache             = cache
            , propertyTransId           = trId
            , propertyValueStream       = stream
            , requestStatus             = mask $ \unmask -> do
                v <- unmask (requestStatus' prop)
                liftIO (atomically $ commitState prop v)
                return v
            , propertyAwait             = mask $ \unmask -> do
                v <- unmask (propertyAwait' prop)
                liftIO (atomically $ commitState prop v)
                return v
            , propertyMutator           = \v -> mkTransition prop (Just v) (propertyMutator' prop v) >>= either (throwPropertyException prop) return
            , propertyMutationPrecheck  = mkPropertyMutationPrecheck extraPart prop
            , propertyMissTargetHandler = mkPropertyMissTargetHandler extraPart prop
            , propertyTimeoutHandler    = mkPropertyTimeoutHandler extraPart prop
            , propertyFailStateHandler  = mkPropertyFailStateHandler extraPart prop }
    return prop

throwPropertyException :: (HasCallStack, StateMap f a, Show (err f a), MonadIO m, MonadThrow m) => Property dev s f a -> err f a -> m b
throwPropertyException prop val = do
    propInfo <- liftIO $ atomically $ takePropertyInfo prop
    throwM $ PropertyException val propInfo

cacheState :: Property dev s f a -> STM (Maybe (f a))
cacheState = readTVar . propertyCache

transitionState :: Property dev s f a -> STM (Maybe (TransitionId dev s f a))
transitionState = readTVar . propertyTransId

transitionInvalidateSTM :: Property dev s f a -> STM ()
transitionInvalidateSTM prop = do
    mval  <- readTVar (propertyTransId prop)
    forM_ mval $ \_ ->
        writeTVar (propertyTransId prop) Nothing

transitionForceInvalidateSTM :: Property dev s f a -> STM ()
transitionForceInvalidateSTM prop = do
    mval  <- readTVar (propertyTransId prop)
    forM_ mval $ \val -> do
        writeTVar (propertyTransId prop) Nothing
        writeTVar (transitionFinalizer val) Nothing

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
    startingStateGuard <|> targetMissmatchCase <|> runningCase <|> lift otherCase <|> error "Unacceptable"
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
                writeTVar (transitionStatus transId) (Right (Just (st, Just (MissTarget (Just target) st))))

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
                            writeTVar (transitionStatus transId) (Right (Just (st, Just $ FailState st)))
                        _      -> return ()

        otherCase = do
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
                    , transitionFinalizer     = error "Unacceptable"
                    }

        cacheReason = localDrivenState st || failState st
        {- || propertyToCache (propertyConf prop) st -}

data WhyNotStart (f :: * -> *) a
    = ZeroMove { oldPos :: a, newPos :: a }
    | PrecheckFail String
    | ValidatorFail String
    deriving (Eq, Show, Typeable)

mkUnsafeTransition2 :: (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> Maybe a -> Action dev Impure s m (TransitionId dev s f a)
mkUnsafeTransition2 prop mtarget = mask_ $ do
    stream   <- liftIO newBroadcastTChanIO
--    timer    <- liftIO $ newTVarIO False
    status   <- liftIO $ newTVarIO $ Right Nothing
    finalVar <- liftIO $ newTVarIO Nothing
    let transition = TransitionId
            { transitionTo          = mtarget
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
    return transition

mkUnsafeTransition :: (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> Maybe a -> f a -> Action dev Impure s m (TransitionId dev s f a)
mkUnsafeTransition prop mtarget start = mask_ $ do
    stream   <- liftIO newBroadcastTChanIO
    timer    <- liftIO $ newTVarIO False
    status   <- liftIO $ newTVarIO $ Left (start, readTVar timer)
    finalVar <- liftIO $ newTVarIO Nothing
    let transition = TransitionId
            { transitionTo          = mtarget
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
    return transition

mkTransition :: (StateMap f a, MonadAction dev n, InScope m (Action dev Impure s n), MonadAction dev m)
    => Property dev s f a -- ^
    -> Maybe a -- ^ Optional transition target
    -> Action dev Impure s m b -- ^ Transition body
    -> Action dev Impure s m (Either (WhyNotStart f a) b)
mkTransition prop mtarget action = withDevice $ \dev -> runExceptT $ do
    forM_ (propertyMutationPrecheck prop dev) $ \precheck -> do
        mval <- lift $ precheck False
        forM_ mval $ \str -> throwE $ PrecheckFail str
    start    <- lift $ askStatus prop
    mtarget' <- forM ((,) <$> mtarget <*> completeState start) $ \(target, old) -> do
        new  <- withExceptT ValidatorFail $ ExceptT $ return $ propertyValidator prop target
        when (propertyEq prop old new) $ throwE (ZeroMove old new)
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

subscribeStatus :: (IfImpure ('R eff 'True), MonadAction dev m) => Property dev s f a -> (STM (f a) -> Action dev ('R eff 'True) s m b) -> Action dev ('R eff 'True) s m b
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


subscribeTransition :: (IfImpure ('R eff 'True), MonadAction dev IO, MonadAction dev m) => Property dev s f a -> TransitionId dev s f a -> (STM (f a) -> Action dev ('R eff 'True) s m b) -> Action dev ('R eff 'True) s m b
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


transitionCompleted :: TransitionId dev s f a -> STM (Maybe (Maybe (TransitionFail f a)))
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


transitionEndAwait :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m) => Property dev s f a -> TransitionId dev s f a -> Action dev ('R eff 'True) s m (Maybe (TransitionFail f a))
transitionEndAwait prop transition = do
    mval <- liftIO $ atomically $ transitionCompleted transition
    merr <- case mval of
        Just v  -> return v
        Nothing -> subscribeTransition prop transition $ \stm -> do
            let conditionS = transitionCompleted transition >>= maybe retry return
                stuckCondition = fix $ \next -> do
                    val <- stm
                    if stateMap val == LocalDrivenIntermediate
                        then return $ Just (StuckState val)
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
                    Just TimeoutFail      -> propertyTimeoutHandler prop
                    Just (FailState _)    -> propertyFailStateHandler prop
                    Just (MissTarget _ _) -> propertyMissTargetHandler prop
                    _                     -> return ()
                liftIO $ atomically $ writeTVar (transitionFinalizer transition) Nothing }
    return merr


-- | Wait current transition complete and start transition to new value
--
-- > writeProperty prop val1
-- > writeProperty prop val2
--
-- transite 'prop' first to 'val1', then to 'val2'.
writeProperty :: (StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> a -> Action dev Impure s m ()
writeProperty prop val = do
    dev <- device
    forM_ (propertyMutationPrecheck prop dev) $ \precheck -> do
        mval <- precheck True
        forM_ mval $ \str -> throwPropertyException prop $ PrecheckFail str
    propertyMutator prop val


propertyTransitionCheck :: (StateMap f a, MonadAction dev m) => Property dev s f a -> Bool -> Action dev Impure s m (Maybe String)
propertyTransitionCheck prop blocking = do
    mtransition <- liftIO $ atomically $ transitionState prop
    case mtransition of
        Nothing         -> return Nothing
        Just transition
            | blocking  -> transitionEndAwait prop transition >>= return . fmap (\v -> propertyName prop ++ ": " ++ show v)
            | otherwise -> do
                val <- liftIO $ atomically $ transitionCompleted transition
                return $ case val of
                    Nothing         -> Just (propertyName prop ++ ": running")
                    Just Nothing    -> Nothing
                    Just (Just err) -> Just (propertyName prop ++ ": " ++ show err)


propertyTransitionCheck' :: (StateMap f a, MonadAction dev m) => (dev s -> Property dev s f a) -> Bool -> Action dev Impure s m (Maybe String)
propertyTransitionCheck' selector blocking = withDevice $ \dev -> propertyTransitionCheck (selector dev) blocking


-- | Wait current transition complete and return completeed value
--
-- > writeProperty prop val
-- > x <- readProperty prop
--
-- 'x' == 'val'
readProperty :: (IfImpure ('R eff 'True), StateMap f a, MonadAction dev m) => Property dev s f a -> Action dev ('R eff 'True) s m a
readProperty prop = do
    res <- liftIO $ atomically $ (,) <$> transitionState prop <*> cacheState prop
    val <- case res of
        (Just transition, _) -> do
            transitionEndAwait prop transition >>= mapM_ (throwPropertyException prop)
            val <- liftIO $ atomically $ do
                Right (Just (val, Nothing)) <- transitionStatusEvent transition
                return val
            return val
        (_, Just val)        -> return val
        _                    -> askStatus prop
    maybe (throwPropertyException prop (MissTarget Nothing val)) return (completeState val)


trackProperty :: (MonadAction dev m, IfImpure ('R eff 'True), StateMap f b)
    => Property dev s f b -- ^
    -> (f b -> Action dev ('R eff 'True) s m ())
    -> Action dev ('R eff 'True) s m ()
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

trackPropertyInfo prop act = do
    propInfo <- takePropertyInfo' prop
    act propInfo
    subscribeStatus prop $ \_ ->
        flip fix propInfo $ \next oldPropInfo -> do
            join $ liftIO $ atomically $ do
                newPropInfo <- takePropertyInfo prop
                when (newPropInfo == oldPropInfo) retry
                return $ do
                    act newPropInfo
                    next newPropInfo



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

class (Typeable a, Typeable f, Show a, Show (f a), Eq a, Eq (f a)) => StateMap f a where
    stateMap :: f a -> TransitionState a

instance (Typeable s, Show s, Eq s) => StateMap TransitionState s where
    stateMap = id

instance (Typeable a, Show a, Eq a) => StateMap Identity a where
    stateMap = Final . runIdentity

instance (Typeable a, Show a, Eq a) => StateMap Maybe a where
    stateMap = maybe RemoteDriven Final

instance (Typeable a, Typeable b, Show a, Show b, Eq a, Eq b) => StateMap (Either b) a where
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
