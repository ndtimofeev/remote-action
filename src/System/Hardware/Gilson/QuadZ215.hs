{-# LANGUAGE DataKinds #-}

module System.Hardware.Gilson.QuadZ215 where

-- base
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Char
import Data.Fixed
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Word

import Text.Read hiding         ( lift )

-- exceptions
-- import Control.Monad.Catch

-- parsec
import Text.Parsec.Char
import Text.Parsec.Combinator   ( option )
import Text.Parsec.Error
import Text.Parsec.Prim         ( parse )
import Text.Parsec.String

-- stm
import Control.Concurrent.STM.TVar
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

data Probe dev s = Probe
    { zAxisPosition    :: Property dev s Position Deci
    , liquidLevelSens  :: Property dev s Identity Word8
    , liquidLevelOut   :: TChan Bool }
    deriving Typeable

data QuadZ s = QuadZ
    { xyAxisPosition   :: Property QuadZ s Position (Deci, Deci)
    , probeWidth       :: Property QuadZ s Position Deci
    , quadZWorkspaces  :: Probes (Range Deci, Range Deci, Range Deci)
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

readRange :: String -> Maybe (Char, (Deci, Deci))
readRange str
    | c:'=':txt <- str
    , (str1, str2) <- span (/='/') txt = do
        v <- (,) <$> readDeci str1 <*> readDeci (tail str2)
        return (c, v)
    | otherwise = Nothing
    where
        readDeci = fmap MkFixed . readMaybe

readRanges :: [String] -> Maybe ((Deci, Deci), (Deci, Deci), (Deci, Deci))
readRanges xs = do
    guard (length xs == 3)
    xs' <- mapM readRange xs
    (,,) <$> lookup 'X' xs' <*> lookup 'Y' xs' <*> lookup 'Z' xs'

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

instance (Typeable a, Eq a) => StateMap ProbePosition a where
    stateMap st = case st of
        ProbeIn v       -> Final v
        ProbeInLiquid v -> OtherComplete v
        ProbeSomewhere  -> Fail
        _               -> LocalDrivenIntermediate

instance (Typeable a, Eq a) => StateMap Position a where
    stateMap st = case st of
        In v      -> Final v
        Somewhere -> Fail
        _         -> RemoteDriven

currentTransitionTarget :: MonadAction dev m => Property dev s f a -> Action dev eff s m (Maybe a)
currentTransitionTarget prop =
    liftIO $ atomically $ runMaybeT $ MaybeT (transitionState prop) >>= MaybeT . return . transitionTo

transPitchStatus :: MonadUnderA m => Property QuadZ s Position Deci -> Action QuadZ eff s m (Position Deci)
transPitchStatus prop = do
    mtarget <- currentTransitionTarget prop
    case mtarget of
        Nothing     -> pitchStatus
        Just target -> do
            let failCond v1 v2 = not $ propertyEq prop v1 v2
            flip fix (10 :: Word8) $ \go n -> do
                stat    <- pitchStatus
                case resultState stat of
                    Just val | n > 0 , failCond val target -> go (n - 1)
                    _                                      -> return stat

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
                return (x + dx - 18, y)

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

probeStatus :: MonadUnderA m => (forall a. Probes a -> a) -> Action QuadZ eff s m (Position Deci)
probeStatus selector = do
    motor <- (selector . probesMotor) <$> motorStatus
    if motor `notElem` [MotorRunning, MotorPowered]
        then return Somewhere
        else do
            mpos <- selector <$> zPosition
            return $ case mpos of
                Just pos
                    | MotorRunning <- motor -> MoveThrough pos
                    | otherwise             -> In pos
                Nothing                     -> Homeing

boxStatus :: MonadUnderA m => Action QuadZ eff s m (Range Deci, Range Deci, Range Deci)
boxStatus = do
    rs <- replicateM 3 $ immediateParsec 'Q' $ rangeParser "XYZ"
    maybe (immediateDecodeError 'Q' (show rs) "") return $
        (,,) <$> lookup 'X' rs <*> lookup 'Y' rs <*> lookup 'Z' rs

rangeParser :: [Char] -> Parser (Char, (Deci, Deci))
rangeParser chars = do
    c  <- oneOf chars
    char '='
    spaces
    n1 <- number
    char '/'
    spaces
    n2 <- number
    return (c, (MkFixed n1, MkFixed n2))


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

mkQZWithNormalBox (xRng, yRng, zRng) = mkQZ (xRng, yRng, fmap (const zRng) Probes {})

mkQZ :: (MonadUnderA m, MonadAccum (CreateCtx d QuadZ s IO) m) => ((Deci, Deci), (Deci, Deci), Probes (Deci, Deci)) -> Action QuadZ Create s m (QuadZ s)
mkQZ box@(xRng, yRng, zRng) = do
    let positionValidate v@(x, y)
            | x < fst xRng || x > snd xRng = Left ("X out of range " ++ show xRng)
            | y < fst yRng || y > snd yRng = Left ("Y out of range " ++ show yRng)
            | otherwise                    = return v

    posProp   <- newGsiocProperty "QuadZ hand position"
        qzPosition
        (\_ (MkFixed x, MkFixed y) -> do
            MkFixed dx <- withDevice $ readProperty . probeWidth
            buffered ("X" ++ show (x + 180 - dx) ++ "/" ++ show y))
        propertyOptional
            { mkPropertyValidator'        = positionValidate
            , mkPropertyEq'               = \(x0, y0) (x1, y1) -> abs (x0 - x1) + abs (y0 - y1) <= 0.2
            }

    widthProp <- newGsiocProperty "Probe distance"
        transPitchStatus
        (\_ (MkFixed val) -> buffered ('w' : show val))
        propertyOptional { mkPropertyValidator' = rangeValidate 9 18 }

    let probeLetter = Probes 'a' 'b' 'c' 'd'

    probesHnd <- forM (Probes (Select aProbe) (Select bProbe) (Select cProbe) (Select dProbe)) $
        \(Select selector) -> do
            zPos <- newGsiocProperty ("Z position " ++ [toUpper (selector probeLetter)] ++ " probe")
                (\_ -> probeStatus selector)
                (\_ (MkFixed val) -> buffered ('Z' : selector probeLetter : show val))
                propertyOptional
                    { mkPropertyValidator' = rangeValidate (fst $ selector zRng) (snd $ selector zRng)
                    , mkPropertyMutationPrecheck' = \_ act -> withDevice $ \quadz -> do
--                        probesEvs <- mapM (propertyCompleteEvent . zAxisPosition) $ probes quadz
--                        widthEv   <- propertyCompleteEvent $ probeWidth quadz
                        propertyCompleteEvent (xyAxisPosition quadz) act
--                        return $ xyEv
--                            c1 <- all id <$> sequence probesEvs
--                            c2 <- widthEv
--                            c3 <- xyEv
--                            return $ c1 && c2 && c3
                    }
            return Probe { zAxisPosition = zPos }

    return QuadZ
        { xyAxisPosition = posProp
        , probeWidth     = widthProp
        , handToHome     = do
        -- Здесь можно инвалидировать транзакции
            liftIO $ atomically $ do
                cacheInvalidateSTM posProp
                transitionInvalidateSTM posProp
                cacheInvalidateSTM widthProp
                transitionInvalidateSTM posProp
                forM_ probesHnd $ \probe -> do
                    cacheInvalidateSTM (zAxisPosition probe)
                    transitionInvalidateSTM (zAxisPosition probe)
            buffered "H"
            mkTransition posProp (Just (fst xRng, fst yRng)) (return ())
            mkTransition widthProp (Just 18) (return ())
            forM_ probesHnd $ \probe -> do
                mkTransition (zAxisPosition probe) Nothing (return ())
            return ()
        , probes          = probesHnd
        , quadZWorkspaces = fmap (\rng -> (xRng, yRng, rng)) zRng }
