module System.Hardware.Thermometer where

-- base
import Control.Applicative

import Data.Char
import Data.Foldable
import Data.Typeable
import Data.Word

-- binary
import Data.Binary.Get

-- bytestring
import qualified Data.ByteString.Char8 as BS ( singleton )
import Data.ByteString ( ByteString )
import Data.ByteString.Builder ( toLazyByteString, char8, word64LE )
import Data.ByteString.Lazy ( fromStrict, toStrict, append, singleton )

-- mtl
import Control.Monad.Reader
import Control.Monad.Reader.Class

-- transformers
import Control.Monad.IO.Class

-- usb
import System.USB

-- vector
import Data.Vector ( indexM )

-- internal
import Control.Monad.Action hiding ( DeviceHandle )

data Thermometer u = Thermometer
    { detectors :: Register Thermometer ThermometerContext u () () [Word64] }
    deriving Typeable

mkThermometer :: MonadUnderA m => Action Thermometer ThermometerContext Create m (Thermometer ())
mkThermometer = do
    ctx <- protocol id
    liftIO $ runReaderT clear ctx
    reg <- newRegister insertDetectors defaultRegister (Complete [])
    return Thermometer { detectors = reg }
    where
        insertDetectors words = do
            ctx <- protocol id
            liftIO $ flip runReaderT ctx $ do
                clear
                mapM_ addDetector words

mkThermometerContext :: MonadUnderA m =>
    Device -> Action NotDevice ThermometerContext Create m ThermometerContext
mkThermometerContext dev = liftIO $ do
    devHnd <- openDevice dev
    setAutoDetachKernelDriver devHnd True
    claimInterface devHnd 0
    inep:outep:_ <- endpoints dev 0 0 0
    trans <- newReadTransfer BulkTransfer devHnd (endpointAddress inep) (maxPacketSize $ endpointMaxPacketSize inep) 20000
    return (ThermometerContext devHnd (endpointAddress outep) trans)

-- getValues :: MonadUnderA m => Action Thermometer ThermometerContext (Impure v) m [Word16]
-- getValues = do
--     protocol id >>=

findDevice :: Word16 -> Word16 -> IO (Maybe Device)
findDevice vid pid = do
    list <- newCtx >>= getDevices >>= mapM (\dev -> do
        desc <- getDeviceDesc dev
        return $ if deviceVendorId desc == vid && deviceProductId desc == pid
            then Just dev
            else Nothing)
    return (foldl (<|>) Nothing list)

endpoints :: Device -> Word8 -> Int -> Int -> IO [EndpointDesc]
endpoints dev configNum interfaceNum interfaceDescNum = do
    configDesc <- getConfigDesc dev configNum
    interface  <- indexM (configInterfaces configDesc) interfaceNum
    indexM interface interfaceDescNum >>= return . toList . interfaceEndpoints

data ThermometerContext = ThermometerContext
    { termometrHandle :: !DeviceHandle
    , outputEndpoint  :: !EndpointAddress
    , readTransfer    :: !ReadTransfer }

clear :: ReaderT ThermometerContext IO (Size, Status)
clear = do
    ctx <- ask
    liftIO $ writeBulk (termometrHandle ctx) (outputEndpoint ctx) (BS.singleton 'C') 100

addDetector :: Word64 -> ReaderT ThermometerContext IO (Size, Status)
addDetector id' = do
    ctx <- ask
    let bs = toStrict $ toLazyByteString $ char8 'A' `mappend` word64LE id'
    liftIO $ writeBulk (termometrHandle ctx) (outputEndpoint ctx) bs 100

detectorResponse :: Get [Word16]
detectorResponse = do
    'D' <- (chr . fromIntegral) <$> getWord8
    n   <- getWord8
    replicateM (fromIntegral (n `div` 2)) getWord16host

takeValue :: ReaderT ThermometerContext IO [Word16]
takeValue = do
    (bs, Completed) <- asks readTransfer >>= liftIO . performReadTransfer
    return $ runGet detectorResponse $ fromStrict bs

withThermometer :: Device -> ReaderT ThermometerContext IO a -> IO a
withThermometer dev act = do
    withDeviceHandle dev $ \hnd -> withDetachedKernelDriver hnd 0 $
        withClaimedInterface hnd 0 $ do
            inep:outep:_ <- endpoints dev 0 0 0
            trans <- newReadTransfer BulkTransfer hnd (endpointAddress inep) (maxPacketSize $ endpointMaxPacketSize inep) 20000
            runReaderT act (ThermometerContext hnd (endpointAddress outep) trans)
