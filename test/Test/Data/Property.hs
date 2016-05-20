module Test.Data.Property where

-- base
import Data.Either
import Data.Fixed
import Data.Typeable

-- random
import System.Random

-- transformers
import Control.Monad.IO.Class

-- mtl
import Control.Monad.Reader

-- QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- internal
import Control.Monad.Action
import Control.Monad.Regions
import Data.Property

deriving instance Random (Fixed a)

writePropertyTest :: (Typeable dev, Arbitrary a, StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> Gen a -> Action dev Impure s m (a, a)
writePropertyTest prop gen = do
    oldVal <- readProperty prop
    val    <- liftIO (generate $ suchThat gen (\newVal -> not (propertyEq prop newVal oldVal)))
    writeProperty prop val
    return (oldVal, val)

writePropertyRandom :: (Typeable dev, Arbitrary a, StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> Action dev Impure s m (a, a)
writePropertyRandom prop = writePropertyTest prop (suchThat arbitrary (isRight . propertyValidator prop))

dispersion :: (Typeable dev, Arbitrary a, StateMap f a, MonadAction dev inner, InScope m (Action dev Impure s inner), MonadAction dev m) => Property dev s f a -> Gen a -> Int -> Action dev Impure s m [(a, a)]
dispersion prop gen num = replicateM num $ do
    (_, target) <- writePropertyTest prop gen
    result      <- readProperty prop
    return (target, result)

frequency' :: [(Int, a)] -> Gen a
frequency' = frequency . map (\(i, v) -> (i, return v))

command :: MonadAction dev m => Gen (Action dev Impure s m a) -> Int -> Action dev Impure s m [a]
command gen num = replicateM num $ join $ liftIO $ generate gen
