module Control.Monad.Catch.Utils where

-- base
import Control.Exception ( PatternMatchFail(..) )

import Data.List

import System.IO.Error

-- exceptions
import Control.Monad.Catch

seConst :: a -> SomeException -> a
seConst = const

handleAny :: MonadCatch m => m a -> m a -> m a
handleAny h = handle (seConst h)

patternFail :: MonadCatch m => m a -> m a -> m a
patternFail h = handle (\(PatternMatchFail _) -> h)

doPatternFail :: MonadCatch m => m a -> m a -> m a
doPatternFail h = handleIOError (\ioe ->
    if isPrefixOf "Pattern match failure in do expression" (ioeGetErrorString ioe)
        then h
        else throwM ioe)

failPattern :: MonadCatch m => m a -> m a -> m a
failPattern act h = catch act (\(PatternMatchFail _) -> h)

catchAny :: MonadCatch m => m a -> m a -> m a
catchAny act = catch act . seConst
