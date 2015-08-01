module System.Hardware.GSIOC.Raw where

-- base
import Prelude

import Control.Applicative
import Control.Concurrent
import Control.Exception ( assert )

import Data.Char
import Data.Typeable
import Data.Maybe
import Data.Word

import System.CPUTime
import System.IO hiding ( hPutStr )

-- mtl
import Control.Monad.Reader

-- serialport
import System.Hardware.Serialport

-- transformers
import Control.Monad.IO.Class

-- exceptions
import Control.Monad.Catch

-- bytestring
import Data.ByteString hiding ( map, any, putStrLn, take, elem, reverse, length )
import qualified Data.ByteString as BS

-- internal
import Control.Concurrent.Utils

-- | Current transaction state information. Only for exception using.
data Transaction
    = Immediate Char String
    | Buffered String String
    | Connect GsiocID
    | Clear
    | CheckBus
    | Unknown
    deriving (Show, Eq)

data RawEnv m = MkRawEnv
    { rawTimeout :: !Integer
    , rawState   :: Transaction
    , rawStep    :: Monad m => Transaction -> RawT m a -> RawT m a
    , rawPutByte :: Word8 -> m ()
    , rawGetByte :: m Word8 }

newtype RawT m a = RawT { runRawT :: ReaderT (RawEnv m) m a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type Raw = RawT IO

data SomeGSIOCException = forall e. Exception e => SomeGSIOCException e
    deriving Typeable

instance Show SomeGSIOCException where
    show (SomeGSIOCException e) = show e

gsiocExceptionToException :: Exception e => e -> SomeException
gsiocExceptionToException = toException . SomeGSIOCException

gsiocExceptionFromException :: Exception e => SomeException -> Maybe e
gsiocExceptionFromException x = do
    SomeGSIOCException e <- fromException x
    cast e

instance Exception SomeGSIOCException

data TimeoutExpired = TimeoutExpired !Transaction !Integer
    deriving (Show, Typeable)

instance Exception TimeoutExpired where
    fromException = gsiocExceptionFromException
    toException = gsiocExceptionToException

data UnexpectedImmediate = UnexpectedImmediate !Char
    deriving (Show, Typeable)

instance Exception UnexpectedImmediate where
    fromException = gsiocExceptionFromException
    toException = gsiocExceptionToException

data WrongEcho = WrongEcho !Transaction !Word8 !Word8
    deriving (Show, Typeable)

instance Exception WrongEcho where
    fromException = gsiocExceptionFromException
    toException = gsiocExceptionToException

data WrongID = WrongID !GsiocID !Word8
    deriving (Show, Typeable)

instance Exception WrongID where
    fromException = gsiocExceptionFromException
    toException = gsiocExceptionToException

-- | GSIOC device ID representation. Each device connected to the GSIOC channel
-- is distinguished by a unique GSIOC identity number (0..63)
type GsiocID = Word8

gsiocFromHandle :: Handle -> (IO Word8, Word8 -> IO ())
gsiocFromHandle hnd = (ioRead, ioWrite)
    where
        ioRead = do
            bs <- hGet hnd 1
            return $ assert (BS.length bs == 1) $ (index bs 0)

        ioWrite byte = hPutStr hnd (singleton byte) >> hFlush hnd

evalRaw :: Integer -> Handle -> Raw a -> IO a
evalRaw t hnd act = evalRaw' t (gsiocFromHandle hnd) act

evalRaw' :: MonadIO m => Integer -> (m Word8, Word8 -> m ()) -> RawT m a -> m a
evalRaw' t (i, o) act = runReaderT (runRawT act) (defaultRawEnw { rawGetByte = i, rawPutByte = o, rawTimeout = t })

evalRaw'' :: MonadIO m => RawEnv m -> RawT m a -> m a
evalRaw'' env act = runReaderT (runRawT act) env


defaultRawEnw :: RawEnv m
defaultRawEnw = MkRawEnv { rawTimeout = 20 ms, rawStep = debugStep, rawState = Unknown }

rawEnwFromHandle :: Handle -> RawEnv IO
rawEnwFromHandle hnd =
    let (ioRead, ioWrite) = gsiocFromHandle hnd
    in  defaultRawEnw { rawGetByte = ioRead, rawPutByte = ioWrite }

step :: Monad m => Transaction -> RawT m a -> RawT m a
step t mval = RawT (asks rawStep) >>= \f -> f t mval

debugStep :: Monad m => Transaction -> RawT m a -> RawT m a
debugStep t mval = RawT $ local (\e -> e { rawState = t }) (runRawT mval)

simpleStep :: Monad m => Transaction -> RawT m a -> RawT m a
simpleStep = const id

putByte :: MonadIO m => Word8 -> RawT m ()
putByte byte = RawT $ asks rawPutByte >>= \f -> lift (f byte)

getByte :: (MonadIO m, MonadMask m) => RawT m Word8
getByte = do
    env <- RawT ask
    let timeoutException = TimeoutExpired (rawState env) (rawTimeout env)
    RawT $ lift $ timeout' timeoutException (rawTimeout env) (rawGetByte env)

-- | Serial port setting for connect to GSIOC over rs232
--
-- * 19200 baud
--
-- * 8 data bits
--
-- * 1 stop bit
--
-- * even
--
-- * no flow control
--
-- * 0.1 second receive timeout
gsiocOverSerialSettings :: SerialPortSettings
gsiocOverSerialSettings = defaultSerialSettings
    { commSpeed = CS19200
    , stopb = One
    , parity = Even
    , flowControl = NoFlowControl }

-- | Disconnect all device on the GSIOC bus. Write 0xFF to GSIOC and wait
-- timeout.
clear :: MonadIO m => RawT m ()
clear = step Clear $ do
    t <- RawT (asks rawTimeout)
    putByte 0xFF
    liftIO $ delay t

-- | Connect to device. Connected device listen immediate and buffered commands.
connectID
    :: (MonadIO m, MonadMask m)
    => GsiocID -- ^ Connected device ID
    -> RawT m ()
connectID id' = step (Connect id') $ do
    putByte (id' + 128)
    v <- getByte
    when (v /= (id' + 128)) $ throwM $ WrongID id' v

scan :: (MonadIO m, MonadMask m) => [GsiocID] -> RawT m [GsiocID]
scan xs = case xs of
    (x:xs') -> do
        mval <- (connectID x >> return (Just x)) `catches`
                    [ Handler (\(WrongID _ _) -> return Nothing)
                    , Handler (\(TimeoutExpired _ _) -> return Nothing) ]
        vals <- scan xs'
        return (maybe vals (:vals) mval)
    _       -> return []


immediate :: (MonadIO m, MonadMask m) => Char -> RawT m String
immediate cmd
    | not (isAscii cmd) = error (show cmd ++ " not ascii")
    | cmd `elem` "#"    = error (show cmd ++ " is special")
    | otherwise    = do
        putByte (ascii cmd)
        str <- go ""
        when (str == "#") $ throwM (UnexpectedImmediate cmd)
        return str
    where
        go acc = step (Immediate cmd (reverse acc)) $ do
            v <- getByte
            if v <= 127
                then putByte 0x06 >> go (toAscii v : acc)
                else
                    let result = reverse (toAscii (v - 128) : acc)
                    in step (Immediate cmd result) (return result)

        toAscii = chr . fromIntegral

-- | Send buffered command to connected device.
buffered :: (MonadIO m, MonadMask m) => String -> RawT m ()
buffered cmd
    | "" <- cmd               = error "Too short command"
    | any (not . isAscii) cmd = error ("Bad cmd " ++ cmd)
    | otherwise               = foldM_ go 1 fullCmd
    where
        fullCmd  = "\n" ++ cmd ++ "\r"

        go acc c = step (Buffered cmd (take acc fullCmd)) $ do
            cond <- writeByte (ascii c)
            if cond
                then return (acc + 1)
                else go acc c

checkBusAvailable :: (MonadIO m, MonadMask m) => RawT m Bool
checkBusAvailable = step CheckBus $ writeByte (ascii '\n')

writeByte :: (MonadIO m, MonadMask m) => Word8 -> RawT m Bool
writeByte byte = do
    trans <- RawT (asks rawState)
    v     <- putByte byte >> getByte
    case v of
        _
            | v == ascii '#'       -> return False
            | v == byte            -> return True
            | otherwise           -> throwM (WrongEcho trans (ascii '\n') v)

ascii :: Char -> Word8
ascii = fromIntegral . ord

observe :: Char -> String -> Raw ()
observe c str = do
    start <- immediate c
    liftIO $ putStrLn start
    buffered str
    let go val = do
            current <- mask_ $ immediate c
            when (current /= val) $ liftIO $ putStrLn current
            go current
    go start
