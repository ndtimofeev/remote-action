-- {-# LANGUAGE Rank2Types #-}

module Control.Concurrent.SigVar where

-- base
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

import Data.Typeable
import Data.List

-- exceptions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class

newtype MVar' a = MVar' (TVar (Either [TVar (Maybe a)] a))

takeMVar' :: MVar' a -> IO a
takeMVar' (MVar' tvar) =
    bracketOnError
        (atomically $ readTVar tvar >>= flip either (return . Right)
            (\xs -> do
                x <- newTVar Nothing
                writeTVar tvar $ Left (x : xs)
                return $ Left x))

        (flip either (const $ return ()) (\ret -> atomically $
            readTVar tvar >>= either (writeTVar tvar . Left . delete ret) (const $ return ())))

        (either (\ret -> atomically $ readTVar ret >>= maybe retry return) return)

putMVar' :: MVar' a -> a -> IO ()
putMVar' (MVar' tvar) v = atomically $ do
    e <- readTVar tvar
    case e of
        Right _ -> retry
        Left [] -> writeTVar tvar (Right v)
        Left xs -> do
            writeTVar tvar (Left (init xs))
            writeTVar (last xs) (Just v)

newtype Subscription a = Subscription (Chan a)

class Listenable pipe where
    mkSubscription :: MonadIO m => pipe a -> m (Subscription a)

instance Listenable Chan where
    mkSubscription = subscriptionForChan

subscriptionForChan :: MonadIO m => Chan a -> m (Subscription a)
subscriptionForChan = liftIO . liftM Subscription . dupChan

listenSubscription :: MonadIO m => Subscription a -> m a
listenSubscription (Subscription chan) = liftIO $ readChan chan

listenPipe :: (MonadIO m, Listenable pipe) => pipe a -> m a
listenPipe pipe = mkSubscription pipe >>= listenSubscription

data SigVar a = SigVar { sigVarState :: !(MVar a), sigVarChan :: !(Chan a) }
    deriving (Eq, Typeable)

instance Listenable SigVar where
    mkSubscription = subscriptionForChan . sigVarChan

newSigVar :: MonadIO m => a -> m (SigVar a)
newSigVar val = liftIO $ do
    chan <- newChan
    mvar <- newMVar val
    return (SigVar mvar chan)

modifySigVar :: (MonadIO m, MonadMask m) => SigVar a -> (a -> m a) -> m ()
modifySigVar (SigVar mvar _) act =
    bracketOnError
        (liftIO (takeMVar mvar))
        (liftIO . putMVar mvar)
        (act >=> liftIO . putMVar mvar)

readSigVar :: MonadIO m => SigVar a -> m a
readSigVar (SigVar mvar _) = liftIO $ readMVar mvar

writeSigVar :: (Eq a, MonadIO m) => SigVar a -> a -> m ()
writeSigVar (SigVar mvar chan) val = liftIO $ do
    oldVal <- takeMVar mvar
    when (val /= oldVal) $ writeChan chan val
    putMVar mvar val

data TSigVar a = TSigVar { tsigVarState :: !(TVar a), tsigVarChan :: !(TChan a) }
    deriving (Eq, Typeable)

newTSigVar :: a -> STM (TSigVar a)
newTSigVar v = do
    var  <- newTVar v
    chan <- newBroadcastTChan
    return TSigVar
        { tsigVarState = var
        , tsigVarChan  = chan }

newTSigVarIO :: MonadIO m => a -> m (TSigVar a)
newTSigVarIO = liftIO . atomically . newTSigVar

listenTSigVar :: TSigVar a -> STM (TChan a)
listenTSigVar = dupTChan . tsigVarChan

listenTSigVarIO :: MonadIO m => TSigVar a -> m (TChan a)
listenTSigVarIO = liftIO . atomically . listenTSigVar

writeTSigVar :: Eq a => TSigVar a -> a -> STM ()
writeTSigVar var v = do
    old <- readTVar (tsigVarState var)
    when (v /= old) $ do
        writeTVar (tsigVarState var) v
        writeTChan (tsigVarChan var) v

readTSigVar :: TSigVar a -> STM a
readTSigVar = readTVar . tsigVarState

readTSigVarIO :: MonadIO m => TSigVar a -> m a
readTSigVarIO = liftIO . atomically . readTSigVar
