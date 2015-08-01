module System.Hardware.Gilson.SystemInterface506C where

-- base
import Control.Monad

import Data.Typeable
import Data.Word

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.Trans.Class

-- mtl
import Control.Monad.Reader.Class

-- internal
import Control.Concurrent.Utils
import Control.Monad.Action

import System.Hardware.GSIOC

newtype PinchStateError = PinchStateError String
    deriving (Show, Typeable)

instance Exception PinchStateError

newtype Output u = Output
    { output :: Register Output GSIOC u () () Bool }
    -- , pulse  :: forall m. MonadUnderA m => Word8 -> forall v. Action Output GSIOC (Impure v) m () }
    deriving Typeable

data SystemInterface506C u = SystemInterface506C
    { output1 :: DeviceHandle Output GSIOC
    , output2 :: DeviceHandle Output GSIOC
    , output3 :: DeviceHandle Output GSIOC
    , output4 :: DeviceHandle Output GSIOC
    , output5 :: DeviceHandle Output GSIOC
    , output6 :: DeviceHandle Output GSIOC }
    deriving Typeable

mkSystemInterface506C :: Action SystemInterface506C GSIOC Create CreateM (SystemInterface506C ())
mkSystemInterface506C = do
    [o1, o2, o3, o4, o5, o6] <- forM [1..6] $ \n -> mkSubDevice $ do
                v   <- (!! (n - 1)) <$> immediate '?'
                reg <- newRegister (writeOutput n) gsiocRegister { regAsk = requestOutput n } (Complete (v == 'C'))
                return Output { output = reg } -- , pulse = pulseN ref 1 }
    return SystemInterface506C
                { output1 = o1
                , output2 = o2
                , output3 = o3
                , output4 = o4
                , output5 = o5
                , output6 = o6 }
    where
        pulseN ref n t = do
            -- atomicWriteIORef ref undefined
            buffered ("P" ++ show n ++ show t)

        writeOutput n b = buffered $ (if b then "C" else "D") ++ show n

        requestOutput n = do
            vec <- immediate '?'
            v   <- lift ask >>= registerValue
            case drop (n - 1) vec of
                []            -> throwM (PinchStateError vec)
                'C':_ | v     -> return $ Complete True
                'D':_ | not v -> return $ Complete False
                _             -> return $ Current ()
