{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Exception where

-- base
import Control.Exception ( SomeException(..), throw )

import Data.Typeable

import GHC.Stack

import System.IO.Unsafe

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.IO.Class

data CallStackException = forall e. Exception e => CallStackException e String

instance Show CallStackException where
    show (CallStackException e str) = show e ++ if str == "" then "" else "\n" ++ str

deriving instance Typeable CallStackException

instance Exception CallStackException where
    displayException (CallStackException e str) =
        displayException e ++ if str == "" then "" else "\n" ++ str

throwIOWithStack :: (HasCallStack, Exception e) => e -> IO a
throwIOWithStack e = do
    stack <- currentCallStack
    throw $ CallStackException e $ if stack /= []
        then prettyCallStack ?callStack ++ "\n" ++ renderStack stack
        else prettyCallStack ?callStack

throwMWithStack :: (HasCallStack, Exception e, MonadIO m) => e -> m a
throwMWithStack = liftIO . throwIOWithStack

throwWithStack :: (HasCallStack, Exception e) => e -> a
throwWithStack = unsafeDupablePerformIO . throwIOWithStack

catchWithStack :: (MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchWithStack eval f = catch eval $ \err@(SomeException top) -> exceptionLoop top f (throw err)
    where
        exceptionLoop :: (Exception a, Exception e) => e -> (a -> m b) -> m b -> m b
        exceptionLoop ex f abort
            | Just (SomeException inner)        <- cast ex = exceptionLoop inner f abort
            | Just (CallStackException inner _) <- cast ex = exceptionLoop inner f abort
            | Just v                            <- cast ex = f v
            | otherwise                                    = abort
