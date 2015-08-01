module Control.Monad.Resumable where

-- base
import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Typeable

-- mtl
import Control.Monad.Reader

-- stm
import Control.Concurrent.STM.TVar

import Control.Monad.STM

-- transformers
import Control.Monad.IO.Class

data ComputationInterrupt = Interrupt deriving (Show, Typeable)

instance Exception ComputationInterrupt

data ComputationState = ComputationState
    { asyncThreadId     :: TVar (Maybe ThreadId)
    , asyncComputation  :: TVar (IO ())
    , asyncFinalization :: SomeException -> STM () }

data AsyncR a = AsyncR
    { asyncState  :: ComputationState
    , asyncResult :: TVar (Maybe (Either SomeException a))
    , asyncForks  :: [ComputationState] }

newtype Resumable a = Resumable { runResumable :: ReaderT (TVar (IO ())) IO a }
    deriving Functor

instance Applicative Resumable where
    pure  = return
    (<*>) = ap

instance Monad Resumable where
    return   = Resumable . return
    mv >>= f = Resumable $ do
        v    <- runResumable mv
        tvar <- ask
        let backup = runResumable (f v)
        lift $ atomically $ writeTVar tvar $ void $ runReaderT backup tvar
        backup

instance MonadIO Resumable where
    liftIO = Resumable . lift

resumableComputation :: Resumable a -> IO (AsyncR a)
resumableComputation eval = do
    tidVar <- newTVarIO Nothing
    backup <- newTVarIO (return ())
    result <- newTVarIO Nothing

    let computation = runReaderT (runResumable eval) backup >>= atomically . writeTVar result . Just . Right

        finalization e
            | Just Interrupt <- fromException e = return ()
            | otherwise                         = writeTVar result (Just (Left e))

    atomically $ writeTVar backup computation
    tid <- forkFinally computation (either (atomically . finalization) return)
    atomically $ writeTVar tidVar $ Just tid
    (atomically $ readTVar result >>= maybe retry return) `onException` killThread tid
    return AsyncR
        { asyncResult = result
        , asyncState  = ComputationState
                            { asyncThreadId     = tidVar
                            , asyncComputation  = backup
                            , asyncFinalization = finalization }
        , asyncForks  = [] }
