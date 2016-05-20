{-# LANGUAGE DataKinds #-}

module System.Hardware.Gilson.QuadZ215 where

-- base
import Control.Applicative
-- import Control.Exception
import Control.Monad

import Data.Char
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Word

import Text.Read hiding         ( lift )
import Text.Printf

-- exceptions
-- import Control.Monad.Catch

-- parsec
import Text.Parsec.Char
import Text.Parsec.Combinator   ( option )
import Text.Parsec.Error
import Text.Parsec.Prim         ( parse )
import Text.Parsec.Perm
import Text.Parsec.String

-- stm
-- import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Control.Monad.STM

-- transformers
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

-- mtl
import Control.Monad.Reader

-- internal
import Control.Concurrent.Utils hiding ( y )
import Control.Monad.Accum
import Control.Monad.Action
import Control.Monad.Regions

import Data.Property

import System.Hardware.GSIOC

rangeValidate :: (Ord a, Show a) => a -> a -> a -> Either String a
rangeValidate minVal maxVal val
    | minVal <= val && maxVal >= val = pure val
    | otherwise                    = Left (show val ++ " out of range (" ++ show minVal ++ "," ++ show maxVal ++ ")")

data Output s = Output
    { output :: Property Output s Maybe Bool
    , pulse  :: forall m. MonadUnderA m => Word8 -> Action Output Impure s m () }
    deriving Typeable

type instance Protocol Output = GSIOC
-- type instance Proto Probe  = GSIOC
type instance Protocol QuadZ  = GSIOC

data NebulaSyringeType
    = Blank                 -- 00000
    | Other                 -- 00001
    | M402                  -- 00002
    | EightSyringeModule    -- 00004
    deriving (Eq, Show, Typeable)

data NebulaZHandModel
    = Hand125mm             -- 0
    | Hand175mm             -- 1
    deriving (Eq, Show, Typeable)

data SpeakerVolume
    = Quiet                 -- 00000
    | Medium                -- 00001
    | Loud                  -- 00002
    deriving (Eq, Show, Typeable)

data SpeakerProfile
    = Sine                  -- 00000
    | Triangle              -- 00001
    deriving (Eq, Show, Typeable)

data NebulaDeviceType
    = N215                  -- 00000
    | N215NOP               -- 16878
    | N215MIC               -- 03373
    | N215MUL               -- 12973
    | N215SPE               -- 05651
    deriving (Eq, Show, Typeable)

data NebulaNVRam = NebulaNVRam
    { nebulaVal00                :: Word16
    , nebulaVal01                :: Word16
    , nebulaVal02                :: Word16
    , nebulaSyringeType          :: NebulaSyringeType
    , nebulaSyringeVolume        :: Word16
    , nebulaZHandModel           :: NebulaZHandModel
    , nebulaEmergencyOutputState :: Word16
    , nebulaEmergencyInputState  :: Word16
    , nebulaSpeakerVolume        :: SpeakerVolume
    , nebulaSpeakerProfile       :: SpeakerProfile
    , nebulaLEDBrightness        :: Word16
    , nebulaWashingStation       :: (Deci, Deci, Deci)
    , nebulaDeviceType           :: NebulaDeviceType
    , nebulaLiquidSensorSens     :: Word16
    , nebulaHeightZHandMount     :: Deci
    , nebulaHomePhase            :: (Deci, Deci)
    , nebulaVal19                :: Word16

    , nebulaVal100               :: Float
    , nebulaVal101               :: Float
    , nebulaVal102               :: Float
    , nebulaVal103               :: Float
    , nebulaVal104               :: Float
    , nebulaVal105               :: Float
    , nebulaVal106               :: Float
    , nebulaVal107               :: Float
    , nebulaVal108               :: Float
    , nebulaVal109               :: Float
    , nebulaVal110               :: Float
    , nebulaVal111               :: Float
    , nebulaVal112               :: Float
    , nebulaVal113               :: Float
    , nebulaVal114               :: Float
    , nebulaVal115               :: Float
    , nebulaVal116               :: Float
    , nebulaVal117               :: Float
    , nebulaVal118               :: Float
    , nebulaVal119               :: Float }
    deriving (Eq, Show, Typeable)

data Probe dev s = Probe
    { zAxisPosition    :: Property dev s Position Deci
    , zAxisSpeed       :: Property dev s Identity Word32
    , liquidLevelSens  :: Property dev s Identity Word8
    , liquidLevelOut   :: TChan Bool }
    deriving Typeable

data QuadZ s = QuadZ
    { xyAxisPosition   :: Property QuadZ s Position (Deci, Deci)
    , probeWidth       :: Property QuadZ s Position Deci
    , quadZWorkspaces  :: (Range Deci, Range Deci, Probes (Range Deci))
    , handToHome       :: forall m inner. (MonadUnderA inner, InScope m (Action QuadZ Impure s inner), MonadUnderA m) => Action QuadZ Impure s m ()
    , probes           :: Probes (Probe QuadZ s) }
    deriving Typeable

data Nebula215 hand s = Nebula215
    { nebula215NVRam   :: Property (Nebula215 hand) s Identity [(Word8, String)]
    , nebula215Output1 :: AccuredDevice s Output
    , nebula215Output2 :: AccuredDevice s Output
    , nebula215Output3 :: AccuredDevice s Output
    , nebula215Output4 :: AccuredDevice s Output
    , nebula215Hand    :: AccuredDevice s hand }
    deriving Typeable

data Nebula215Table = Nebula215Table
    { rack1         :: (Deci, Deci)
    , rack2         :: (Deci, Deci)
    , rack3         :: (Deci, Deci)
    , rack4         :: (Deci, Deci)
    , rack5         :: (Deci, Deci)
    , rinseStation1 :: (Deci, Deci) }
    deriving (Eq, Show, Typeable)

data Rack222 = Rack222
    { positions :: [((Word, Word), (Deci, Deci))] }
    deriving (Eq, Show, Typeable)

data Position a
    = In a
    | MoveThrough a
    | Homeing
    | Somewhere
    deriving (Eq, Ord, Show, Typeable)

data ProbePosition a
    = ProbeIn a
    | ProbeInLiquid a
    | ProbeMoveThrough a
    | ProbeHoming
    | ProbeSomewhere
    deriving (Eq, Ord, Show, Typeable)

type Range a = (a, a)

instance (Typeable a, Show a, Eq a) => StateMap ProbePosition a where
    stateMap st = case st of
        ProbeIn v       -> Final v
        ProbeInLiquid v -> OtherComplete v
        ProbeSomewhere  -> Fail
        _               -> LocalDrivenIntermediate

instance (Typeable a, Show a, Eq a) => StateMap Position a where
    stateMap st = case st of
        In v      -> Final v
        Somewhere -> Fail
        _         -> RemoteDriven

currentTransitionTarget :: MonadAction dev m => Property dev s f a -> Action dev eff s m (Maybe a)
currentTransitionTarget prop =
    liftIO $ atomically $ runMaybeT $ MaybeT (transitionState prop) >>= MaybeT . return . transitionTo

nebulaNVRamStatus :: (MonadAction dev m, Protocol dev ~ GSIOC) => Action dev eff s m (Either [String] NebulaNVRam)
nebulaNVRamStatus = do
    buffered "@00"
    w  <- fix $ \onemore -> do
        w <- immediate '@'
        case w of
            '0' : '0' : _ -> return w
            _             -> onemore
    ws <- replicateM 19 $ immediate '@'
    buffered "@100"
    f  <- fix $ \onemore -> do
        f <- immediate '@'
        case f of
            '1' : '0' : '0' : _ -> return f
            _                   -> onemore
    fs <- replicateM 19 $ immediate '@'
    return $ maybe (Left (ws ++ fs)) Right $ do
        ws' <- zipWithM stripPrefix (map (printf "%02d=") ([0..19] :: [Word8])) (w : ws) >>= mapM readMaybe
        fs' <- zipWithM stripPrefix (map (printf "%03d=") ([100..119] :: [Word8])) (f : fs) >>= mapM readMaybe
        decodeNebulaNVRam (ws', fs')

decodeNebulaNVRam :: ([Word16], [Float]) -> Maybe NebulaNVRam
decodeNebulaNVRam (ws, fs) = do
    guard (length ws == 20 && length fs == 20)

    syrType        <- case ws !! 3 of
        0 -> return Blank
        1 -> return Other
        2 -> return M402
        4 -> return EightSyringeModule
        _ -> Nothing

    zHndModel      <- case ws !! 5 of
        0 -> return Hand125mm
        1 -> return Hand175mm
        _ -> Nothing

    speakerVolume  <- case ws !! 8 of
        0 -> return Quiet
        1 -> return Medium
        2 -> return Loud
        _ -> Nothing

    speakerProfile <- case ws !! 9 of
        0 -> return Sine
        1 -> return Triangle
        _ -> Nothing

    devType        <- case ws !! 14 of
        0     -> return N215
        16878 -> return N215NOP
        03373 -> return N215MIC
        12973 -> return N215MUL
        05651 -> return N215SPE
        _     -> Nothing

    return NebulaNVRam
        { nebulaVal00                = ws !! 0
        , nebulaVal01                = ws !! 1
        , nebulaVal02                = ws !! 2
        , nebulaSyringeType          = syrType
        , nebulaSyringeVolume        = ws !! 4
        , nebulaZHandModel           = zHndModel
        , nebulaEmergencyOutputState = ws !! 6
        , nebulaEmergencyInputState  = ws !! 7
        , nebulaSpeakerVolume        = speakerVolume
        , nebulaSpeakerProfile       = speakerProfile
        , nebulaLEDBrightness        = ws !! 10
        , nebulaWashingStation       = (wordToMM (ws !! 11), wordToMM (ws !! 12), wordToMM (ws !! 13))
        , nebulaDeviceType           = devType
        , nebulaLiquidSensorSens     = ws !! 15
        , nebulaHeightZHandMount     = wordToMM $ ws !! 16
        , nebulaHomePhase            = (wordToMM (ws !! 17), wordToMM (ws !! 18))
        , nebulaVal19                = ws !! 19

        , nebulaVal100               = fs !! 0
        , nebulaVal101               = fs !! 1
        , nebulaVal102               = fs !! 2
        , nebulaVal103               = fs !! 3
        , nebulaVal104               = fs !! 4
        , nebulaVal105               = fs !! 5
        , nebulaVal106               = fs !! 6
        , nebulaVal107               = fs !! 7
        , nebulaVal108               = fs !! 8
        , nebulaVal109               = fs !! 9
        , nebulaVal110               = fs !! 10
        , nebulaVal111               = fs !! 11
        , nebulaVal112               = fs !! 12
        , nebulaVal113               = fs !! 13
        , nebulaVal114               = fs !! 14
        , nebulaVal115               = fs !! 15
        , nebulaVal116               = fs !! 16
        , nebulaVal117               = fs !! 17
        , nebulaVal118               = fs !! 18
        , nebulaVal119               = fs !! 19 }
    where
        wordToMM = MkFixed . fromIntegral

stablePitchStatus :: MonadUnderA m => Word -> Action QuadZ eff s m (Position Deci)
stablePitchStatus retryNum = do
    pos <- pitchStatus
    case pos of
        In val -> stable retryNum pos pitchStatus return
            (\pos' -> case pos' of
                In v -> return $ MoveThrough v
                _    -> return pos'
            )
        _      -> return pos

stable :: (Monad m, Eq a) => Word -> a -> m a -> (a -> m b) -> (a -> m b) -> m b
stable num old eval good bad
    | num < 1   = good old
    | otherwise = do
        new <- eval
        if new == old
            then stable (num - 1) old eval good bad
            else bad new


pitchStatus :: MonadUnderA m => Action QuadZ eff s m (Position Deci)
pitchStatus = do
    digest <- motorStatus
    mpos   <- readMaybe <$> immediate 'w'
    return $ case pitchMotor digest of
        MotorPowered | Just val <- mpos -> In $ MkFixed val
        MotorRunning | Just val <- mpos -> MoveThrough $ MkFixed val
        MotorNotInit                    -> Somewhere
        _                               -> Homeing


qzPosition :: (IfImpure ('R eff 'True), MonadUnderA m) => Property QuadZ s f a -> Action QuadZ ('R eff 'True) s m (Position (Deci, Deci))
qzPosition _ = do
    digest <- motorStatus
    let xyMotor = xMotor digest <> yMotor digest <> pitchMotor digest
    if xyMotor `notElem` [MotorRunning, MotorPowered]
        then return Somewhere
        else do
            mpos <- runMaybeT $ do
                x  <- MaybeT $ immediateParsec 'X' $ value 5
                y  <- MaybeT $ immediateParsec 'Y' $ value 5
                dx <- MaybeT $ withDevice $ \dev -> completeState <$> askStatus (probeWidth dev)
                return (x - aProbe probesShift dx, y)

            return $ case mpos of
                Just pos
                    | MotorRunning <- xyMotor -> MoveThrough pos
                    | otherwise               -> In pos
                Nothing                       -> Homeing

value :: Int -> Parser (Maybe Deci)
value n = (replicateM_ n (char '?') $> Nothing) <|> (Just . MkFixed <$> number)

number :: Parser Integer
number = do
    spaces
    sign <- option "" $ string "-"
    num  <- some digit
    return $ read (sign ++ num)

data Probes a = Probes { aProbe :: a, bProbe :: a, cProbe :: a, dProbe :: a }
    deriving (Eq, Ord, Show, Typeable, Foldable, Functor, Traversable)

data Select f = Select (forall a. f a -> a)

data MotorDigest a = MotorDigest
    { xMotor      :: a
    , yMotor      :: a
    , probesMotor :: Probes a
    , pitchMotor  :: a }
    deriving (Eq, Ord, Show, Typeable, Foldable, Functor, Traversable)

data MotorStatus = MotorPowered | MotorRunning | MotorUnpowered | MotorNotInit | MotorError
    deriving (Eq, Ord, Show, Typeable)

instance Monoid MotorStatus where
    mempty      = MotorPowered
    mappend l r = fromMaybe MotorPowered $ msum $
        (\v -> guard (v == l || v == r) >> pure v) <$> [MotorError, MotorNotInit, MotorUnpowered,MotorRunning]

immediateParsec :: (Typeable dev, MonadUnderA m, Protocol dev ~ GSIOC) => Char -> Parser a -> Action dev eff s m a
immediateParsec cmd parser = do
    str <- immediate cmd
    case parse parser str str of
        Right v  -> return v
        Left err -> immediateDecodeError cmd str (intercalate "; " $ messageString <$> errorMessages err)


zPosition :: MonadUnderA m => Action QuadZ eff s m (Probes (Maybe Deci))
zPosition = immediateParsec 'Z' $ Probes
    <$> value 4 <*> (char ',' >> value 4) <*> (char ',' >> value 4) <*> (char ',' >> value 4)


zSpeed :: MonadUnderA m => Action QuadZ eff s m (Probes Integer)
zSpeed = immediateParsec 'O' $ Probes
    <$> number <*> (char ',' >> number) <*> (char ',' >> number) <*> (char ',' >> number)


probeStatus :: MonadUnderA m => Word -> (forall a. Probes a -> a) -> Action QuadZ eff s m (Position Deci)
probeStatus retryNum selector = do
    motor <- (selector . probesMotor) <$> motorStatus
    if motor `notElem` [MotorRunning, MotorPowered]
        then return Somewhere
        else do
            mpos <- selector <$> zPosition
            case mpos of
                Just pos
                    | MotorRunning <- motor -> return $ MoveThrough pos
                    | otherwise             ->
                        stable retryNum mpos
                            (selector <$> zPosition)
                            (return . maybe Homeing In)
                            (return . maybe Homeing MoveThrough)
                Nothing                     -> return $ Homeing

boxStatus :: MonadUnderA m => Action QuadZ eff s m (Range Deci, Range Deci, Range Deci)
boxStatus = do
    rs <- replicateM 3 $ immediate 'Q'
    case parse (permute $ (,,) <$$> rangeParser 'X' <||> rangeParser 'Y' <||> rangeParser 'Z') (concat rs) (concat rs) of
        Left err -> immediateDecodeError 'Q' (concat rs) (intercalate "; " $ messageString <$> errorMessages err)
        Right v  -> return v

rangeParser :: Char -> Parser (Deci, Deci)
rangeParser c = do
    char c
    char '='
    spaces
    n1 <- number
    char '/'
    spaces
    n2 <- number
    return (MkFixed n1, MkFixed n2)


motorStatus :: MonadUnderA m => Action QuadZ eff s m (MotorDigest MotorStatus)
motorStatus = immediateParsec 'm' $ (\xM yM aM bM cM dM pM -> MotorDigest xM yM (Probes aM bM cM dM) pM)
    <$> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus
    where
        parseStatus
              = (char 'P' $> MotorPowered)
            <|> (char 'R' $> MotorRunning)
            <|> (char 'U' $> MotorUnpowered)
            <|> (char 'E' $> MotorError)
            <|> (char 'I' $> MotorNotInit)

probeLetter :: Probes Char
probeLetter = Probes 'a' 'b' 'c' 'd'

probesShiftF :: Fractional a => a -> Probes (a -> a)
probesShiftF offset = Probes (\w -> offset - 1.5 * w) (\w -> offset - 0.5 * w) (\w -> offset + 0.5 * w) (\w -> offset + 1.5 * w)

probesShift :: Probes (Deci -> Deci)
probesShift = probesShiftF 27

probeXRange :: (IfImpure ('R eff 'True), MonadUnderA m) => (forall a. Probes a -> a) -> Action QuadZ ('R eff 'True) s m (Deci, Deci)
probeXRange selector = withDevice $ \dev -> do
    w <- readProperty $ probeWidth dev
    let shift                = selector probesShift w
        ((xMin, xMax), _, _) = quadZWorkspaces dev
    return (xMin + shift, xMax + shift)

probePosition :: (IfImpure ('R eff 'True), MonadUnderA m) => (forall a. Probes a -> a) -> Action QuadZ ('R eff 'True) s m (Deci, Deci, Deci)
probePosition selector = withDevice $ \dev -> do
    w      <- readProperty $ probeWidth dev
    (x, y) <- readProperty $ xyAxisPosition dev
    z      <- readProperty $ zAxisPosition $ selector $ probes dev
    return (x + selector probesShift w, y, z)

moveTo :: (MonadAction QuadZ inner, InScope m (Action QuadZ Impure s inner), MonadAction QuadZ m)
    => (forall a. Probes a -> a) -- ^ Probe selector
    -> Deci -- ^ Safe z-arm height
    -> (Deci, Deci, Deci) -- ^ Target z-arm position
    -> Action QuadZ Impure s m ()
moveTo selector safeZ (x, y, z) = withDevice $ \dev -> do
    w     <- readProperty $ probeWidth dev
    let shiftedX = x - selector probesShift w
    oldXY <- readProperty $ xyAxisPosition dev
    unless (propertyEq (xyAxisPosition dev) oldXY (shiftedX, y)) $ do
        forM_ (probes dev) $ \p -> do
            h <- readProperty (zAxisPosition p)
            when (not (propertyEq (zAxisPosition p) h safeZ) && h < safeZ) $ do
                writeProperty (zAxisPosition p) safeZ
        writeProperty (xyAxisPosition dev) (shiftedX, y)
    oldZ  <- readProperty (zAxisPosition $ selector $ probes dev)
    unless (propertyEq (zAxisPosition $ selector $ probes dev) oldZ z) $ do
        writeProperty (zAxisPosition $ selector $ probes dev) z

handToHome' :: (MonadAction QuadZ inner, InScope m (Action QuadZ Impure s inner), MonadAction QuadZ m) => Action QuadZ Impure s m ()
handToHome' = withDevice $ \dev -> do
    liftIO $ atomically $ do
        cacheInvalidateSTM (xyAxisPosition dev)
        transitionInvalidateSTM (xyAxisPosition dev)
        cacheInvalidateSTM (probeWidth dev)
        transitionInvalidateSTM (probeWidth dev)
        forM_ (probes dev) $ \probe -> do
            cacheInvalidateSTM (zAxisPosition probe)
            transitionInvalidateSTM (zAxisPosition probe)
    buffered "E000"
    buffered "H"
    mkTransition (xyAxisPosition dev) Nothing (return ())
    mkTransition (probeWidth dev) Nothing (return ())
    forM_ (probes dev) $ \probe -> mkTransition (zAxisPosition probe) Nothing (return ())
    return ()

mkQZWithNormalBox :: (MonadUnderA m, MonadAccum (CreateCtx d QuadZ s IO) m) => ((Deci, Deci), (Deci, Deci), (Deci, Deci)) -> Action QuadZ Create s m (QuadZ s)
mkQZWithNormalBox (xRng, yRng, zRng) = mkQZ (xRng, yRng, Probes zRng zRng zRng zRng)

mkQZ :: (MonadUnderA m, MonadAccum (CreateCtx d QuadZ s IO) m) => ((Deci, Deci), (Deci, Deci), Probes (Deci, Deci)) -> Action QuadZ Create s m (QuadZ s)
mkQZ box@(xRng, yRng, zRng) = do
    let positionValidate v@(x, y)
            | x < fst xRng || x > snd xRng = Left (show v ++ " X out of range " ++ show xRng)
            | y < fst yRng || y > snd yRng = Left (show v ++ " Y out of range " ++ show yRng)
            | otherwise                    = return v

    posProp   <- newGsiocProperty "QuadZ hand position"
        qzPosition
        (\_ (MkFixed x, MkFixed y) -> do
            dx <- withDevice $ readProperty . probeWidth
            let MkFixed offset = aProbe probesShift dx
            buffered ("X" ++ show (x + offset) ++ "/" ++ show y))
        propertyOptional
            { mkPropertyValidator        = positionValidate
            , mkPropertyEq               = \(x0, y0) (x1, y1) -> abs (x0 - x1) + abs (y0 - y1) <= 0.3
            , mkPropertyMutationPrecheck = \_ dev -> propertyTransitionCheck' probeWidth : (toList $ fmap (propertyTransitionCheck . zAxisPosition) $ probes dev)
            }

    widthProp <- newGsiocProperty "Probe distance"
        (\_ -> stablePitchStatus 10)
        (\_ (MkFixed val) -> buffered ('w' : show val))
        propertyOptional
            { mkPropertyValidator = rangeValidate 9 18
            , mkPropertyMutationPrecheck = \_ dev -> propertyTransitionCheck' xyAxisPosition : (toList $ fmap (propertyTransitionCheck . zAxisPosition) $ probes dev)
            }

    let speedPatterns = Probes (++",,,") (\str -> "," ++ str ++ ",,") (\str -> ",," ++ str ++ ",") (",,,"++)

    probesHnd <- forM (Probes (Select aProbe) (Select bProbe) (Select cProbe) (Select dProbe)) $
        \(Select selector) -> do

            zAxisPosition' <- newGsiocProperty ("Z position " ++ [toUpper (selector probeLetter)] ++ " probe")
                (\_ -> probeStatus 5 selector)
                (\_ (MkFixed val) -> buffered ('Z' : selector probeLetter : show val))
                propertyOptional
                    { mkPropertyValidator = rangeValidate (fst $ selector zRng) (snd $ selector zRng)
                    , mkPropertyMutationPrecheck = \_ dev ->
                        propertyTransitionCheck' probeWidth :
                        propertyTransitionCheck' xyAxisPosition :
                            (toList $ fmap (propertyTransitionCheck . zAxisPosition) $ probes dev) }

            zAxisSpeed'    <- newGsiocProperty ("Speed of probe " ++ [toUpper (selector probeLetter)] ++ " probe")
                (\_     -> do
                    val <- selector <$> zSpeed
                    return $ pure $ fromInteger val)
                (\_ w32 -> buffered ("O" ++ selector speedPatterns (show w32)))
                propertyOptional

            return Probe { zAxisPosition = zAxisPosition', zAxisSpeed = zAxisSpeed' }

    return QuadZ
        { xyAxisPosition = posProp
        , probeWidth     = widthProp
        , handToHome     = do
        -- Здесь можно инвалидировать транзакции
            liftIO $ atomically $ do
                cacheInvalidateSTM posProp
                transitionForceInvalidateSTM posProp
                cacheInvalidateSTM widthProp
                transitionForceInvalidateSTM widthProp
                forM_ probesHnd $ \probe -> do
                    cacheInvalidateSTM (zAxisPosition probe)
                    transitionForceInvalidateSTM (zAxisPosition probe)
            mkUnsafeTransition2 posProp Nothing -- (Just (fst xRng, fst yRng))
            mkUnsafeTransition2 widthProp Nothing
            forM_ probesHnd $ \probe ->
                mkUnsafeTransition2 (zAxisPosition probe) Nothing
            digest <- motorStatus
            if any (==MotorRunning) digest
                then do
                    delay (200 ms)
                --    buffered "E000"
                --    buffered "E111"
                    buffered "SH"
                else buffered "H"
        , probes          = probesHnd
        , quadZWorkspaces = box }
