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

catchAny :: MonadCatch m => m a -> m a -> m a
catchAny act = catch act . seConst
