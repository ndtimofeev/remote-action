module System.Hardware.Gilson.Pump306 where

-- base
import Control.Concurrent
import Control.Monad

import Data.Typeable
import Data.Word

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- mtl
import Control.Monad.Reader.Class

-- internal
import Control.Monad.Action

import System.Hardware.GSIOC

data OutOfRange a = OutOfRange { valueOutOfRange :: a, minRangeBound :: a, maxRangeBound :: a, outOfRangeComment :: String }

data Pump306 s = Pump306
    { p306HeadVolume :: Word
    , p306Speed      :: Register Pump306 GSIOC s () () Word
    , p306RefillTime :: Register Pump306 GSIOC s () () Word }
    deriving Typeable

flowWithSpeed :: MonadUnderA m => Word -> Action Pump306 GSIOC Mut m ()
flowWithSpeed s = withDevice $ \pump ->
    (registerWrite' (p306Speed pump) s >> forever (liftIO $ threadDelay maxBound)) `finally` registerWrite' (p306Speed pump) 0

mkPump306 :: MonadUnderA m => Word -> Action Pump306 GSIOC Create m (Pump306 ())
mkPump306 headVolume = do
    buffered "L"
    reg1 <- liftM (Complete . rateToFlow . read) (immediate 's') >>= newRegister writeSpeed gsiocRegister { regAsk = speedRequest }
    reg2 <- liftM (Complete . read) (immediate 'R') >>= newRegister writeRefillTime gsiocRegister { regAsk = refillTimeRequest }
    return (Pump306 headVolume reg1 reg2)
    where
        flowToRate flow = flow * 10000 `div` headVolume

        rateToFlow rate = headVolume * rate `div` 10000

        refillTimeRequest :: MonadRegister dev GSIOC s () () Word m => Action dev GSIOC eff m (TransactionValue () () Word)
        refillTimeRequest = do
            r <- immediate 'R'
            v <- lift ask >>= registerValue
            return $ if (v == read r)
                then Complete v
                else Current ()

        speedRequest :: MonadRegister dev GSIOC s () () Word m => Action dev GSIOC eff m (TransactionValue () () Word)
        speedRequest = do
            r <- immediate 's'
            v <- lift ask >>= registerValue
            return $ if (v == rateToFlow (read r))
                then Complete v
                else Current ()

        writeRefillTime time
            | time < 125 || time > 1000 = error "refill time out of range"
            | otherwise                 = buffered ('R':show time)

        writeSpeed speed
            | flowToRate speed > 12272 = error "speed out of range"
            | otherwise                = buffered ('s':show (flowToRate speed))
