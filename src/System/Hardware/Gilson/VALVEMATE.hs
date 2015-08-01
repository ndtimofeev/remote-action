module System.Hardware.Gilson.VALVEMATE where

-- base
import Control.Exception ( throw )
import Control.Monad

import Data.Typeable
import Data.Word
import Data.Ix
import Data.List

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Reader.Class

-- internal
import Control.Monad.Action
import Control.Monad.Statemachine
import Control.Monad.Catch.Utils

import System.Hardware.GSIOC

data NVRamCheckFail = NVRamCheckFail TypeRep
    deriving (Show, Typeable)

data MissingMotorStep = MissingMotorStep TypeRep
    deriving (Show, Typeable)

data HomePositionError = HomePositionError TypeRep
    deriving (Show, Typeable)

data IllegalPosition = IllegalPosition TypeRep
    deriving (Show, Typeable)

data Absurd = Absurd String
    deriving (Show, Typeable)

instance Exception NVRamCheckFail
instance Exception MissingMotorStep
instance Exception HomePositionError
instance Exception IllegalPosition
instance Exception Absurd

data ValvePosition = A | B | C | D | E | F | G | H
    deriving (Eq, Show, Ord, Enum, Typeable)

data VALVEMATE i u = VALVEMATE
    { vlvPositionNumber  :: Word
    , vlvCurrentPosition :: Register (VALVEMATE i) GSIOC u () i i
    , vlvToHome          :: forall m. MonadUnderA m => forall v. Action (VALVEMATE i) GSIOC (Impure v) m () }
    deriving Typeable

mkVALVEMATE :: (Enum i, Typeable i, MonadUnderA m) => Word -> Action (VALVEMATE i) GSIOC Create m (VALVEMATE i ())
mkVALVEMATE num
    | num > 8   = error ("Positions: " ++ show num)
    | otherwise = do
        buffered "L"
        pos <- requestCurrentPosition
        cts <- return $ case pos of
            Complete v  -> v
            Current v   -> v
            Undefined _ -> undefined
        reg <- newRegister writeCurrentPosition gsiocRegister { regAsk = requestCurrentPosition } (Undefined cts)
        return (VALVEMATE num reg (buffered "H"))
    where
        vlvToHome ::
            (Enum i, Typeable i, MonadUnderA m) => Action (VALVEMATE i) GSIOC eff m ()
        vlvToHome = doPatternFail (return ()) $ do
            Undefined _ <- requestCurrentPosition
            buffered "H"

        writeCurrentPosition pos = do
            lift ask >>= registerWait
            buffered ['P', ['A'..'H'] !! fromEnum pos]

        requestCurrentPosition ::
            (Enum i, Typeable i, MonadUnderA m) => Action (VALVEMATE i) GSIOC eff m (TransactionValue () i i)
        requestCurrentPosition = do
            [status, alphaPos] <- immediate 'P'
            let pos = toEnum $ index ('A', 'H') alphaPos
            case status of
                'P'                  -> return $ Complete pos
                'M' | alphaPos == '-' -> return $ Undefined ()
                'M'                  -> return $ Current pos
                'E'                  -> do
                    trp <- getTypeRep
                    join $ immediateEnum 'E'
                        [ ("00", throwM $ Absurd (show trp ++ ": current position is Error, but error code is \"No error\""))
                        , ("10", throwM $ NVRamCheckFail trp)
                        , ("11", throwM $ MissingMotorStep trp)
                        , ("12", throwM $ HomePositionError trp)
                        , ("14", throwM $ IllegalPosition trp) ]

data VALVEMATE' u = VALVEMATE'
    { vlvPositionNumber'  :: Word8
    , vlvCurrentPosition' :: StateM VALVEMATE' GSIOC u Maybe Word8 }
    deriving Typeable

mkVALVEMATE' :: MonadUnderA m => Word8 -> Action VALVEMATE' GSIOC Create m (VALVEMATE' ())
mkVALVEMATE' posNum = do
    st <- newStateM requestPos targetPos undefined undefined undefined
    return VALVEMATE' { vlvPositionNumber' = posNum, vlvCurrentPosition' = st }
    where
        targetPos :: MonadUnderA m => Transition VALVEMATE' GSIOC l eff Maybe Word8 m (Maybe Word8)
        targetPos = do
            str <- immediate 'P'
            case str of
                ['M', pos]
                    | Just i <- elemIndex pos ['A'..'H'] -> return $ Just $ fromIntegral i
                    | '-' <- pos                        -> error "Homing"
                    | otherwise                         -> error ""
                _          -> return Nothing

        requestPos :: MonadUnderA m => Transition VALVEMATE' GSIOC l eff Maybe Word8 m (Maybe Word8)
        requestPos = undefined
