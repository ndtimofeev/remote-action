module Data.Property.Test where

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
import Data.Property

deriving instance Random (Fixed a)

transiteToRandom :: (Typeable dev, Result f a, MonadUnderA m) => Gen a -> Transition dev (Impure eff) s f a m a
transiteToRandom gen = do
    rv <- liftIO (generate gen)
    transiteTo rv
    return rv

transiteToRandom' :: (Typeable dev, Arbitrary a, Result f a, MonadUnderA m) => Transition dev (Impure eff) s f a m a
transiteToRandom' = do
    prop <- ask
    transiteToRandom (suchThat arbitrary (isRight . propertyValid (getPropertyMeta prop)))

writePropertyRandom :: (Typeable dev, Arbitrary a, Result f a, MonadUnderA m) => Property dev s f a -> Action dev (Impure eff) m a
writePropertyRandom prop = do
    val <- liftIO (generate $ suchThat arbitrary (isRight . propertyValid (getPropertyMeta prop)))
    writeProperty prop val
    return val
