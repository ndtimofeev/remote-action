:set -XDataKinds
:set -XScopedTypeVariables
:set -XKindSignatures
:set -XRecordWildCards

:m + Data.Word Data.IORef
:m + Data.Fixed Data.Proxy GHC.TypeLits
:m + Control.Concurrent.Async
:m + Data.Time.Clock
:m + Control.Monad.IO.Class
:m + Control.Monad
:m + Control.Monad.STM Control.Concurrent.STM.TVar Control.Concurrent.STM.TMVar

:m + Control.Concurrent.Utils
:m + Control.Monad.Action
:m + System.Hardware.Serialport System.Hardware.GSIOC
:m + System.Hardware.Gilson.SyringePump402
:m + System.Hardware.Thermometer System.USB

:set prompt "EyMgBr> "

type Precision (n :: Nat) = Fixed (Proxy n)

instance KnownNat n => HasResolution (Proxy n) where resolution _ = 10 ^ natVal (undefined :: Proxy n)

:{
let tty                 = "/dev/ttyUSB0"
    toBottle            = (Reservoir, Reservoir)
    toReactor           = (Needle, Needle)
    maxReactorT         = 50
    maxRefluxT          = 25
    maxRoomT            = 25
    minSpeed            = 2
    maxSpeed            = 13
    deltaT              = 2
    minDeltaT           = 1

    mkTherm :: Word16 -> Word16 -> [Word64] -> IO Device
    mkTherm vid pid ids = do
        Just therm <- findDevice vid pid
        withThermometer therm $ do
            clear
            mapM_ addDetector ids
        return therm

    getCurrentTemeratures :: Device -> IO [Precision 4]
    getCurrentTemeratures therm = withThermometer therm (map (\v -> fromIntegral v / 16) <$> takeValue)

    reactorOverheat t   = t >= maxReactorT

    setSpeed speed      = withDevice $ \SyringePump402 {..} -> do
        syringesHome toBottle
        registerWrite' sp402syrsCurrentSpeed (maxSpeed + 1, maxSpeed + 1)
        registerWrite' sp402syrsCurrentSpeed (speed, speed)
:}

bus   <- hOpenSerial tty gsiocOverSerialSettings >>= openBus 20000
syr   <- mkDevice (mkGsiocProtocol bus 4) (mkSyringePump402 (5000, 5000) (7, 7))
therm <- mkTherm 0x0483 0x1337 [0x2B0000064D85A428, 0xD90000064D38EB28, 0xEB0000064D6AE428]
task  <- async (return ()) >>= newIORef -- :: IO (IORef (Async ()))
tell  <- newEmptyTMVarIO :: IO (TMVar DiluteAnyFromCmd)
incTs <- getCurrentTime >>= newIORef

:{
let start speed = do
        mres <- readIORef task >>= poll
        case mres of
            Just _  -> async woker >>= writeIORef task
            Nothing -> atomically (putTMVar tell (Speed speed))
        liftIO $ putStrLn ("Speed " ++ show speed)
        where
            woker = unsafeAction syr (setSpeed speed >> diluteAnyFrom2 tell 1500000 toBottle toReactor)

    stop        = atomically (putTMVar tell Pause)

    ignition vol speed = do
        start maxSpeed
        timer <- async (delay (vol * 60 * 1000000 `div` maxSpeed) >> start speed)
        let loop = do
                ts <- withThermometer therm (map (\v -> fromIntegral v / 16) <$> takeValue)
                print ts
                case ts of
                    [_, _, reactor]
                        | reactor > 30 -> cancel timer >> stop
                        | otherwise    -> loop
        loop
        putStrLn "Ignition!"
        Control.Concurrent.Utils.timeout (5 m) tloop
        flow speed

    overheat = do
        ts <- withThermometer therm (map (\v -> fromIntegral v / 16) <$> takeValue)
        print ts
        putStrLn "Overheat!"
        case ts of
            [high, low, reactor]
                | reactor < 50
                && (low - high > 1 || (low < maxRoomT && high > maxRoomT)) -> decrease >>= flow
                | otherwise                      -> overheat

    currentSpeed = pureAction syr $ withDevice $ \SyringePump402{..} -> do
        (v, _) <- registerRead sp402syrsCurrentSpeed
        return v

    increase = do
        speed <- currentSpeed
        return $ case () of
            _ | speed < minSpeed -> minSpeed
              | speed > maxSpeed -> maxSpeed
              | otherwise        -> speed + 1

    decrease = do
        speed <- currentSpeed
        return $ case () of
            _ | speed <= minSpeed -> minSpeed
              | speed > maxSpeed -> maxSpeed
              | otherwise        -> speed - 1

    flow speed = do
        putStrLn "Flow!"
        start speed
        let loop = do
                ts <- withThermometer therm (map (\v -> fromIntegral v / 16) <$> takeValue)
                print ts
                case ts of
                    [high, low, reactor]
                        | reactor < 30 -> ignition 50000 5
                        | reactor > 50 || low - high < 1 -> stop >> overheat
                        | high < maxRoomT && low < maxRoomT -> loop
                        | low - high < 2 -> do
                            ct <- getCurrentTime
                            ot <- readIORef incTs
                            -- when (diffUTCTime ct ot > 120) (decrease >>= start >> writeIORef incTs ct)
                            when (diffUTCTime ct ot > 120) $ do
                                nv <- decrease
                                putStrLn "Decrease"
                                atomically (putTMVar tell (Speed nv))
                                writeIORef incTs ct
                            loop
                        | low - high > 4 && reactor < 45 -> do
                            ct <- getCurrentTime
                            ot <- readIORef incTs
                            -- when (diffUTCTime ct ot > 240) (increase >>= start >> writeIORef incTs ct)
                            when (diffUTCTime ct ot > 180) $ do
                                nv <- increase
                                putStrLn "Increase"
                                atomically (putTMVar tell (Speed nv))
                                writeIORef incTs ct
                            loop
                        | otherwise -> loop
                    _  -> undefined -- bad term
        loop

    tloop = withThermometer therm $ forever $
        map (\v -> fromIntegral v / 16) <$> takeValue >>= liftIO . print
:}
