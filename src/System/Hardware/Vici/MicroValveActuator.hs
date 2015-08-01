module System.Hardware.Vici.MicroValveActuator where

-- base
import Control.Monad

import Data.List
import Data.Maybe
import Data.Typeable

import Numeric.Natural

import System.IO
import System.IO.Error

import Text.Read

-- exceptions
import Control.Monad.Catch

-- serialport
-- import System.Hardware.Serialport

-- stm
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class

-- internal
import Control.Concurrent.Utils

import Control.Monad.Action
import Control.Monad.Statemachine

data Vici = Vici { viciErrorNum :: TVar Natural, viciFile :: Handle }

data ShortResponce = ShortResponce !Natural [String]
    deriving (Show, Typeable)

data WrongResponce = WrongResponce String [String]
    deriving (Show, Typeable)

instance Exception ShortResponce
instance Exception WrongResponce

mkViciProtocol :: MonadUnderA m => Handle -> Action NotDevice Vici Create m Vici
mkViciProtocol hnd = do
    errNum <- liftIO $ newTVarIO 0
    return Vici { viciErrorNum = errNum, viciFile = hnd }

writeCmd :: MonadUnderA m => String -> Action dev Vici eff m ()
writeCmd str = do
    protocol viciFile >>= liftIO . flip hPutStr (str ++ "\r")
    delay (2 ms)

readResponse :: MonadUnderA m => Natural -> Action dev Vici eff m [String]
readResponse i = do
    xs <- protocol viciFile >>= liftIO . fmap split . hGetLine
    case genericLength xs of
        n
            | n < i     -> throwM (ShortResponce i xs)
            | n == i     -> return xs
            | otherwise -> do
                errNum <- protocol viciErrorNum
                liftIO $  atomically $ modifyTVar' errNum (+(n - i))
                return $ genericDrop (n - i) xs
    where
        split = lines . map (\c -> if c == '\r' then '\n' else c)

decodeResponce :: MonadUnderA m => String -> ([String] -> Maybe a) -> [String] -> Action dev Vici eff m a
decodeResponce request decoder response =
    maybe (throwM (WrongResponce request response)) return (decoder response)


data MicroValveActuator s
    = MicroValveActuator
    { programVersion  :: (String, String)
    , positionRange   :: (Word, Word)
    , currentPosition :: StateM MicroValveActuator Vici s Maybe Word }
    deriving Typeable

mkMicroValveActuator :: MonadUnderA m => Action MicroValveActuator Vici Create m (MicroValveActuator ())
mkMicroValveActuator = do
    -- init device
    writeCmd "vr"
    [firmwareVersion, firmwareDate] <- readResponse 2
    -- read position number
    writeCmd "np"
    np  <- readResponse 1 >>= decodeResponce "np" (listToMaybe >=> stripPrefix "NP = " >=> readMaybe)
    cp  <- positionRequest
    reg <- newStateM undefined undefined undefined undefined (Just (Just cp))
    return MicroValveActuator
        { programVersion  = (firmwareVersion, firmwareDate)
        , positionRange   = (1, np)
        , currentPosition = reg }
    where
        positionRequest = do
            writeCmd "cp"
            xs <- readResponse 1
            decodeResponce "cp" (listToMaybe >=> stripPrefix "Position is  = " >=> readMaybe) xs

        -- tryPositionRequest :: MonadUnderA m => Action dev Vici eff m (Maybe Word)
        tryPositionRequest =
            catchIf isEOFError (Just <$> positionRequest) (const (return Nothing))
