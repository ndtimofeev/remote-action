module Control.Concurrent.Utils
    ( USecond, TimeMult(..), us, ms, s, m, h, d, w, y, timeout, timeout', delay, never )
where

-- base
import Control.Concurrent
-- import Control.Exception ( throw, throwTo )
import Control.Monad

import Data.List
import Data.Unique
import Data.Typeable
import Data.Ratio

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.IO.Class

type USecond = Integer

newtype Timeout = Timeout { timeoutUid :: Unique }
    deriving Typeable

instance Show Timeout where show _ = "Timeout"
instance Exception Timeout

newtype TimeMult = TimeMult { usPerQuant :: Integer }

us, ms, s, m, h, d, w, y :: TimeMult

-- | microsecond
us = TimeMult 1

-- | millisecond
ms = TimeMult 1000

-- | second
s  = TimeMult 1000000

-- | minute — 60 s
m  = TimeMult (60 * usPerQuant s)

-- | houre — 60 m
h  = TimeMult (60 * usPerQuant m)

-- | day — 24 h
d  = TimeMult (24 * usPerQuant h)

-- | week — 7 d
w  = TimeMult (7 * usPerQuant d)

-- | year — 365 d
y  = TimeMult (365 * usPerQuant d)

instance Num a => Fractional (TimeMult -> a) where
    fromRational r (TimeMult k) = fromInteger (numerator r * k `div` denominator r)
    recip =undefined -- _

instance Num a => Num (TimeMult -> a) where
    fromInteger i (TimeMult k) = fromInteger (i * k)
    (+)    = undefined -- _
    (-)    = undefined -- _
    (*)    = undefined -- _
    abs    =undefined -- _
    signum =undefined -- _

delay :: MonadIO m => USecond -> m ()
delay t = liftIO $ do
    sequence_ $ genericReplicate n $ threadDelay maxTime
    threadDelay (fromInteger m')
    where
        (n, m') = t `divMod` toInteger maxTime

        maxTime = maxBound

never :: MonadIO m => m ()
never = liftIO $ forever $ threadDelay maxBound

timeout' :: (MonadIO m, MonadMask m) => Exception e => e -> USecond -> m a -> m a
timeout' ex t act = do
    tid  <- liftIO myThreadId
    bracket
        (liftIO $ forkIO $ delay t >> throwTo tid ex)

        (uninterruptibleMask_ . liftIO . killThread)

        (const act)

timeout :: (MonadIO m, MonadMask m) => USecond -> m a -> m (Maybe a)
timeout t act = do
    tid <- liftIO myThreadId
    uid <- liftIO newUnique
    bracket
        (liftIO $ forkIO $ delay t >> throwTo tid (Timeout uid))

        (uninterruptibleMask_ . liftIO . killThread)

        (\_ -> catchIf (\e -> uid == timeoutUid e) (Just <$> act) (\_ -> return Nothing))

-- uninterruptibleMaskWithTimeout :: (MonadIO m, MonadMask m, Exception e) => e -> Integer -> m a -> m a
-- uninterruptibleMaskWithTimeout ex t act = uninterruptibleMask $ \restore -> do
--     mvar <- liftIO newEmptyMVar :: MonadIO m => m (MVar (Either SomeException a))
--     liftIO $ forkIOWithUnmask $ \unmask -> unmask $ try (timeout' ex t act) >>= liftIO . putMVar mvar
--     liftIO $ takeMVar mvar >>= restore . either throwM return
