module System.Hardware.Gilson.QuadZ215 where

-- base
import Control.Applicative
import Control.Monad

import Data.Fixed
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Word

import Text.Read hiding         ( lift )

-- parsec
import Text.Parsec.Char
import Text.Parsec.Combinator   ( option )
import Text.Parsec.Error
import Text.Parsec.Prim         ( parse )
import Text.Parsec.String

-- stm
import Control.Concurrent.STM.TChan

import Control.Monad.STM

-- transformers
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

-- mtl
import Control.Monad.Reader

-- internal
import Control.Monad.Accum
import Control.Monad.Action

import Data.Property

import System.Hardware.GSIOC

rangeValidate :: (Ord a, Show a) => a -> a -> a -> Either String a
rangeValidate minVal maxVal val
    | minVal <= val && maxVal >= val = pure val
    | otherwise                    = Left (show val ++ " out of range (" ++ show minVal ++ "," ++ show maxVal ++ ")")

data Output s = Output
    { output :: Property Output s Maybe Bool
    , pulse  :: forall m. MonadUnderA m => Word8 -> Action Output (Impure s) m () }
    deriving Typeable

type instance Proto Output = GSIOC
type instance Proto Probe  = GSIOC
type instance Proto QuadZ  = GSIOC

data Probe s = Probe
    { zAxisPosition    :: Property Probe s (Either Deci) Deci
    , liquidLevelSens  :: Property Probe s Identity Word8
    , liquidLevelOut   :: TChan Bool }
    deriving Typeable

data QuadZ s = QuadZ
    { xyAxisPosition   :: Property QuadZ s Position (Deci, Deci)
    , probeWidth       :: Property QuadZ s Position Deci
    , quadZWorkspace   :: (Range Deci, Range Deci, Range Deci)
    , handToHome       :: forall m. MonadUnderA m => forall eff. Action QuadZ (Impure eff) m ()
    , probeA           :: AccuredDevice s Probe
    , probeB           :: AccuredDevice s Probe
    , probeC           :: AccuredDevice s Probe
    , probeD           :: AccuredDevice s Probe }
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

type Range a = (a, a)

instance Eq a => Result Position a where
    resultState st = case st of
        In v -> Just v
        _    -> Nothing

    failState st = st == Somewhere

transPitchStatus :: MonadUnderA m => Transition QuadZ eff s Position Deci m (Position Deci)
transPitchStatus = do
    prop    <- ask
    mtarget <- currentTransitionTarget
    case mtarget of
        Nothing     -> pitchStatus
        Just target ->
            let failCond v1 v2 = not $ propertyEq (getPropertyMeta prop) v1 v2
                go n           = do
                    stat    <- pitchStatus
                    case resultState stat of
                        Just val | n > 0 , failCond val target -> go (n - 1)
                        _                                      -> return stat

            in go (10 :: Word)

pitchStatus :: MonadUnderA m => Action QuadZ eff m (Position Deci)
pitchStatus = do
    digest <- motorStatus
    mpos   <- readMaybe <$> immediate 'w'
    return $ case pitchMotor digest of
        MotorPowered | Just val <- mpos -> In $ MkFixed val
        MotorRunning | Just val <- mpos -> MoveThrough $ MkFixed val
        MotorNotInit                    -> Somewhere
        _                               -> Homeing


qzPosition :: (IfImpure eff, MonadUnderA m) => Action QuadZ eff m (Position (Deci, Deci))
qzPosition = do
    digest <- motorStatus
    let xyMotor = xMotor digest <> yMotor digest <> pitchMotor digest
    if xyMotor `notElem` [MotorRunning, MotorPowered]
        then return Somewhere
        else do
            mpos <- runMaybeT $ do
                x  <- MaybeT (immediateParsec 'X' value)
                y  <- MaybeT (immediateParsec 'Y' value)
                dx <- lift $ withDevice $ readProperty . probeWidth
                return (x + dx - 18, y)

            return $ case mpos of
                Just pos
                    | MotorRunning <- xyMotor -> MoveThrough pos
                    | otherwise               -> In pos
                Nothing                       -> Homeing
    where
        value  = (string "?????" $> Nothing) <|> (Just . MkFixed <$> number)
        number = do
            spaces
            sign <- option "" $ string "-"
            num  <- some digit
            return $ read (sign ++ num)

data MotorDigest a = MotorDigest
    { xMotor      :: a
    , yMotor      :: a
    , aProbeMotor :: a
    , bProbeMotor :: a
    , cProbeMotor :: a
    , dProbeMotor :: a
    , pitchMotor  :: a }
    deriving (Eq, Ord, Show, Typeable, Foldable, Functor, Traversable)

data MotorStatus = MotorPowered | MotorRunning | MotorUnpowered | MotorNotInit | MotorError
    deriving (Eq, Ord, Show, Typeable)

instance Monoid MotorStatus where
    mempty      = MotorPowered
    mappend l r = fromMaybe MotorPowered $ msum $
        (\v -> guard (v == l || v == r) >> pure v) <$> [MotorError, MotorNotInit, MotorUnpowered,MotorRunning]

immediateParsec :: (Typeable dev, MonadUnderA m, Proto dev ~ GSIOC) => Char -> Parser a -> Action dev eff m a
immediateParsec cmd parser = do
    str <- immediate cmd
    case parse parser str str of
        Right v  -> return v
        Left err -> immediateDecodeError cmd str (intercalate "; " $ messageString <$> errorMessages err)

motorStatus :: (Typeable dev, MonadUnderA m, Nominal dev ~ QuadZ, Proto dev ~ GSIOC) => Action dev eff m (MotorDigest MotorStatus)
motorStatus = immediateParsec 'm' $ MotorDigest
    <$> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus <*> parseStatus
    where
        parseStatus
              = (char 'P' $> MotorPowered)
            <|> (char 'R' $> MotorRunning)
            <|> (char 'U' $> MotorUnpowered)
            <|> (char 'E' $> MotorError)
            <|> (char 'I' $> MotorNotInit)

defaultGSIOCTracker ::
    (Result f a, MonadUnderA m) =>
    STM Bool ->
    (f a -> Transition dev Pure s f a m ()) -> Transition dev Pure s f a m ()
defaultGSIOCTracker tracked _ = ask >>= \prop -> forever $ do
    liftIO $ atomically $ do
        tracked >>= guard
        cacheState prop >>= guard . isNothing
    void requestStatus

gsiocVariable
    :: (MonadUnderA m, MonadAccum (CreateCtx d (Action dev Create IO)) m, Result f a)
    => PropertyMeta a -- ^ Name, validator and equal for property. 
    -> (forall m1 eff. (IfImpure eff, MonadUnderA m1) => Transition dev eff () f a m1 (f a)) -- ^ Property unblockable state acess
    -> (forall m1. MonadUnderA m1 => a -> forall eff. Transition dev (Impure eff) () f a m1 ()) -- ^ Start transition to new value
    -> Action (New dev) Create m (Property dev () f a)
gsiocVariable meta getter mutator = do
    prop <- newProperty $ do
        return
            ( PropertyConf
                { propertyMeta    = meta
                , propertyToCache = const False
                , propertyGetter  = getter
                , propertyMutator = mutator
                , propertyTracker = Just defaultGSIOCTracker }
            , Nothing )
    addToStage $ void $ withProperty prop $ requestStatus
    return prop

mkQZ :: CreateAction QuadZ (QuadZ ())
mkQZ = do
    xs <- atomic $ replicateM 3 $ immediate 'Q'
    let errMsg              = "Can't parse " ++ concat xs
        box@(xRng, yRng, _) = fromMaybe (error errMsg) (readRanges xs)
        positionValidate v  = case v of
            (x, y)
                | x < fst xRng || x > snd xRng -> Left "X out of range"
                | y < fst yRng || y > snd yRng -> Left "Y out of range"
                | otherwise                    -> return v

    posProp   <- gsiocVariable
        PropertyMeta
            { propertyName  = "QuadZ hand position"
            , propertyValid = positionValidate
            , propertyEq    = \(x0, y0) (x1, y1) -> abs (x0 - x1) + abs (y0 - y1) <= 0.2 }
        qzPosition
        (\(MkFixed x, MkFixed y) -> do
            MkFixed dx <- withDevice $ readProperty . probeWidth
            buffered ("X" ++ show (x + 180 - dx) ++ "/" ++ show y))

    widthProp <- gsiocVariable
        pMeta { propertyName = "Probe distance", propertyValid = rangeValidate 9 18  }
        transPitchStatus
        (\(MkFixed val) -> buffered ('w' : show val))
    return QuadZ
        { xyAxisPosition = posProp
        , probeWidth     = widthProp
        , handToHome     = do
            buffered "H"
            withProperty posProp $ cacheInvalidate
            withProperty widthProp $ cacheInvalidate
        , probeA         = undefined
        , probeB         = undefined
        , probeC         = undefined
        , probeD         = undefined
        , quadZWorkspace = box }
