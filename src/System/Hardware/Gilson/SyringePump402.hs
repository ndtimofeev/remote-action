module System.Hardware.Gilson.SyringePump402 where

-- base
import Control.Concurrent.MVar
-- import Control.Exception
import Control.Monad

import Data.Typeable
import Data.Word
import Data.Ix
import Data.IORef

-- async
import Control.Concurrent.Async

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Reader.Class

-- internal
import Control.Monad.Action

import System.Hardware.GSIOC

data ValvePos = Reservoir | Needle
    deriving (Eq, Ix, Ord, Bounded, Show, Read, Typeable)

type Run a  = Either a a
type Sym a  = (a, a)
type Run' a = (a, Bool)

data SyringePump402 u = SyringePump402
    { sp402syrsVolume         :: (Word, Word)
    , sp402syrsValvePos       :: Register SyringePump402 GSIOC u (Maybe ValvePos, Maybe ValvePos) (Maybe ValvePos, Maybe ValvePos) (ValvePos, ValvePos)
    , sp402syrsCurrentSpeed   :: Register SyringePump402 GSIOC u () () (Word, Word)
-- | Complete  — position
--
--   Current   — position × paused
--
--   Undefined — not init + position × paused
    , sp402syrsAspirateVolume :: Register SyringePump402 GSIOC u () (Run' Word, Run' Word) (Word, Word)
    , sp402syrsSyringesHome   :: forall m. MonadUnderA m => forall v. Action SyringePump402 GSIOC (Impure v) m () }
    deriving Typeable

currentSyringsPosition :: MonadUnderA m => Action SyringePump402 GSIOC eff m (Word, Word)
currentSyringsPosition = withDevice $ \syr -> do
    cts <- registerAsk (sp402syrsAspirateVolume syr)
    return $ case cts of
        Complete v                 -> v
        Current ((lv, _), (rv, _)) -> (lv, rv)
        Undefined _                -> error "Not initialized"

paused :: MonadUnderA m => Action SyringePump402 GSIOC eff m Bool
paused = withDevice $ \syr -> do
    cts <- registerAsk (sp402syrsAspirateVolume syr)
    return $ case cts of
        Current ((_, p1), (_, p2)) -> not p1 && not p2
        Undefined _                -> error "Not initialized"
        _                          -> False

halt :: MonadUnderA m => Action SyringePump402 GSIOC (Impure v) m ()
halt = buffered "HB"

resume :: MonadUnderA m => Action SyringePump402 GSIOC (Impure v) m ()
resume = paused >>= \p -> when p $ buffered "BB"

homed :: MonadUnderA m => Action SyringePump402 GSIOC eff m Bool
homed = withDevice $ \syr -> do
    cts <- registerAsk (sp402syrsAspirateVolume syr)
    return $ case cts of
        Undefined _ -> False
        _           -> True

syringesHome :: MonadUnderA m => (ValvePos, ValvePos) -> Action SyringePump402 GSIOC Mut m ()
syringesHome pos = withDevice $ \syr -> do
    registerWrite' (sp402syrsValvePos syr) pos
    sp402syrsSyringesHome syr
    let wait = do
            str <- immediate 'M'
            case splitAt 6 str of
                ('I':_, _) -> wait
                (_, 'I':_) -> wait
                _          -> return ()
    wait
    (sl, sr) <- registerRead (sp402syrsCurrentSpeed syr)
    buffered ("SL" ++ show sl)
    buffered ("SR" ++ show sr)

diluteAnyFrom :: MonadUnderA m => Integer -> (ValvePos, ValvePos) -> (ValvePos, ValvePos) -> Action SyringePump402 GSIOC Mut m ()
diluteAnyFrom i (leftFrom, rightFrom) (leftTo, rightTo) = withDevice $ \syrs -> do
    currentSyringsPosition >>= \pos -> when (pos /= (0, 0)) $ error "Syringes maybe not empty"
    let (leftVolume, rightVolume) = sp402syrsVolume syrs
        volumeReg                 = sp402syrsAspirateVolume syrs
        valvesReg                 = sp402syrsValvePos syrs
        go volume left
            | volume < 0 = return ()
            | left = do
                registerWrite' valvesReg (leftFrom, rightTo)
                registerWrite' volumeReg (min (fromInteger volume) leftVolume, 0)
                when (volume > 0) $ go (max (volume - toInteger leftVolume) 0) False
            | otherwise = do
                registerWrite' valvesReg (leftTo, rightFrom)
                registerWrite' volumeReg (0, min (fromInteger volume) rightVolume)
                when (volume > 0) $ go (max (volume - toInteger rightVolume) 0) True
    go i True `onException` halt

data DiluteAnyFromCmd = Speed Word | Pause | Resume deriving (Show, Eq, Ord)

diluteAnyFrom2 :: MonadUnderA m => TMVar DiluteAnyFromCmd -> Integer -> (ValvePos, ValvePos) -> (ValvePos, ValvePos) -> Action SyringePump402 GSIOC Mut m ()
diluteAnyFrom2 tmvar i (leftFrom, rightFrom) (leftTo, rightTo) = withDevice $ \syrs -> do
    currentSyringsPosition >>= \pos -> when (pos /= (0, 0)) $ error "Syringes maybe not empty"
    let (leftVolume, rightVolume) = sp402syrsVolume syrs
        volumeReg                 = sp402syrsAspirateVolume syrs
        valvesReg                 = sp402syrsValvePos syrs
        next volume cont          = do
            await <- pureAsync (registerWait volumeReg)
            let loop = do
                    event <- liftIO $ atomically $ (waitSTM await >> return Nothing) `orElse` (Just <$> takeTMVar tmvar)
                    case event of
                        Nothing | volume > 0 -> cont
                        Just (Speed v)       -> do
                            buffered "HB"
                            registerWrite' (sp402syrsCurrentSpeed syrs) (v, v)
                            buffered "BB"
                            loop
                        Just Pause           -> buffered "HB" >> loop
                        Just Resume          -> buffered "BB" >> loop
                        _                    -> return ()
            loop
        go volume left
            | volume < 0 = return ()
            | left = do
                registerWrite' valvesReg (leftFrom, rightTo)
                registerWrite volumeReg (min (fromInteger volume) leftVolume, 0)
                next volume (go (max (volume - toInteger leftVolume) 0) False)
            | otherwise = do
                registerWrite' valvesReg (leftTo, rightFrom)
                registerWrite' volumeReg (0, min (fromInteger volume) rightVolume)
                next volume (go (max (volume - toInteger rightVolume) 0) True)
    go i True `onException` halt

mkSyringePump402 :: MonadUnderA m => (Word, Word) -> (Word, Word) -> Action SyringePump402 GSIOC Create m (SyringePump402 ())
mkSyringePump402 volumes@(leftSyrVolume, rightSyrVolume) speeds@(leftSyrSpeed, rightSyrSpeed) = do
    buffered $ "PL" ++ show leftSyrVolume
    buffered $ "PR" ++ show rightSyrVolume
    buffered $ "SL" ++ show leftSyrSpeed
    buffered $ "SR" ++ show rightSyrSpeed
    ipos             <- immediate 'V' >>= \str ->
                    return $ case mapM charToMaybeValve str of
                        Just [p1, p2] -> Complete (p1, p2)
                        _             -> Undefined (Reservoir, Reservoir)
    vPosReg          <- newRegister writeSyrsPos gsiocRegister { regAsk = syrsPosRequest } ipos
    sSpeedReg        <- newRegister writeSpeed gsiocRegister (Complete speeds)
    (sVolumReg, ref) <- newRegisterWithRef writeAspirateVolume gsiocRegister { regAsk = aspirateVolumeRequest } (Undefined (0, 0))
    return (SyringePump402 volumes vPosReg sSpeedReg sVolumReg (buffered "OB" >> liftIO (atomicWriteIORef ref (Current (0, 0)))))
    where
        writeSyrsPos (lpos, rpos) = do
            reg <- lift ask
            cst <- registerAsk reg
            case cst of
                Undefined _ -> waitStop reg
                _           -> registerWait reg
            buffered $ 'V' : 'L' : valveToChar lpos : []
            buffered $ 'V' : 'R' : valveToChar rpos : []

        writeSpeed (lspeed, rspeed) = do
            buffered $ "SL" ++ show lspeed
            buffered $ "SR" ++ show rspeed

        writeAspirateVolume v@(lvol, rvol)
            | lvol > leftSyrVolume || rvol > rightSyrVolume = error "Imposible"
            | otherwise = do
                reg <- lift ask
                cts <- transactionState reg
                case cts of
                    Undefined _ -> error "Not initialized"
                    _           -> do
                        cv@(v1, v2) <- registerRead reg
                        when (cv /= v) $ do
                            when (v1 /= lvol) $ buffered $ if v1 > lvol then "DL" ++ show (v1 - lvol) else "AL" ++ show (lvol - v1)
                            when (v2 /= rvol) $ buffered $ if v2 > rvol then "DR" ++ show (v2 - rvol) else "AR" ++ show (rvol - v2)
                            buffered "BB"

        syrsPosRequest ::
            MonadRegister dev GSIOC s (Maybe ValvePos, Maybe ValvePos) (Maybe ValvePos, Maybe ValvePos) (ValvePos, ValvePos) m =>
            Action dev GSIOC eff m (TransactionValue (Maybe ValvePos, Maybe ValvePos) (Maybe ValvePos, Maybe ValvePos) (ValvePos, ValvePos))
        syrsPosRequest = do
            cts   <- lift ask >>= transactionState
            l:r:_ <- immediate 'V'
            return $ case (cts, stateValue cts) of
                (Undefined _, _)  -> Undefined (charToMaybeValve l, charToMaybeValve r)
                (_, (ltar, rtar))
                    | l `elem` "OM" || r `elem` "OM" -> error "Boom!"
                    | l == 'X'|| r == 'X'
                        || charToValve l /= ltar
                        || charToValve r /= rtar -> Current (charToMaybeValve l, charToMaybeValve r)
                    | otherwise                 -> Complete (charToValve l, charToValve r)

        aspirateVolumeRequest ::
            MonadRegister dev GSIOC s () (Run' Word, Run' Word) (Word, Word) m =>
            Action dev GSIOC eff m (TransactionValue () (Run' Word, Run' Word) (Word, Word))
        aspirateVolumeRequest = do
            str <- immediate 'M'
            tar <- lift ask >>= registerValue
            return $ case splitAt 6 str of
                (l:lnum, r:rnum)
                    | [l,r] `elem` ["HH", "HN", "NH"] -> Current ((read lnum, False), (read rnum, False))
                    | l == 'R' || r == 'R' -> Current ((read lnum, True), (read rnum, True))
                    | l == 'N' && r == 'N'
                    && tar == (read lnum, read rnum) -> Complete (read lnum, read rnum)
                    -- -- | l == 'N' || r == 'N' -> Current (Right (read lnum, read rnum))
                    -- -- | l == 'I' || r == 'I' -> Undefined ()
                    | otherwise          -> Undefined ()

        waitStop reg = registerAsk reg >>= \c -> case c of
            Undefined (Just _, Just _) -> return ()
            Current (Just _, Just _)   -> return ()
            Complete _                 -> return ()
            _                          -> waitStop reg

        charToValve :: Char -> ValvePos
        charToValve c = case c of
            'R' -> Reservoir
            'N' -> Needle
            _   -> undefined

        charToMaybeValve :: Char -> Maybe ValvePos
        charToMaybeValve c = case c of
            'R' -> Just Reservoir
            'N' -> Just Needle
            _   -> Nothing

        valveToChar :: ValvePos -> Char
        valveToChar pos = case pos of
            Reservoir -> 'R'
            Needle    -> 'N'
