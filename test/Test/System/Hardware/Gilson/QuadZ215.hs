module Test.System.Hardware.Gilson.QuadZ215 where

-- base
import Data.Fixed

import Data.Foldable

-- QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe

-- internal
import Control.Monad.Action
import Control.Monad.Regions

import Data.Property

import System.Hardware.Gilson.QuadZ215

-- internal-test
import Test.Control.Monad.Action

import Test.Data.Property

xyPositionGen :: QuadZ s -> Gen (Deci, Deci)
xyPositionGen dev =
    let (xRng, yRng, _) = quadZWorkspaces dev
    in  (,) <$> choose xRng <*> choose yRng

movePositionGen :: (IfImpure ('R eff 'True), MonadAction QuadZ m) => (forall a. Probes a -> a) -> Action QuadZ ('R eff 'True) s m (Gen (Deci, Deci, Deci))
movePositionGen selector = do
    dev  <- device
    xRng <- probeXRange selector
    let (_, yRng, probeRngs) = quadZWorkspaces dev
    return $ (,,) <$> choose xRng <*> choose yRng <*> choose (selector probeRngs)

quadZTraverse :: Monad m => QuadZ s -> (forall f a. StateMap f a => Property QuadZ s f a -> m b) -> m [b]
quadZTraverse dev act = do
    v1 <- act $ xyAxisPosition dev
    v2 <- act $ probeWidth dev
    vs <- mapM (act . zAxisPosition) $ probes dev
    return (v1 : v2 : toList vs)

probeGen :: Gen (Select Probes)
probeGen = elements [Select aProbe, Select bProbe, Select cProbe, Select dProbe]

quadZScriptGen :: (MonadAction QuadZ inner, InScope m (Action QuadZ Impure s inner), MonadAction QuadZ m)
    => QuadZ s
    -> Gen (Action QuadZ Impure s m (Action QuadZ Impure s m (), LogEntity))
quadZScriptGen dev = frequency
    [ (1, return $ return (handToHome dev, Entity "Hand to home"))
    , (5, do
        posGen <- promote $ do
            old <- readProperty (xyAxisPosition dev)
            return $ suchThat (xyPositionGen dev) (/= old)
        return $ do
            pos <- posGen
            return (writeProperty (xyAxisPosition dev) pos, Entity (propertyName (xyAxisPosition dev), pos)))
    , (5, do
        Select probe <- probeGen
        posGen       <- promote $ do
            let (_, _, probeRngs) = quadZWorkspaces dev
            old <- readProperty (zAxisPosition $ probe $ probes dev)
            return (suchThat (choose (probe probeRngs)) (/=old))
        return $ do
            pos <- posGen
            return (writeProperty (zAxisPosition $ probe $ probes dev) pos, Entity (propertyName (zAxisPosition $ probe $ probes dev), pos)))
    , (5, do
        Select probe <- probeGen
        posGen       <- promote (movePositionGen probe)
        return $ do
            pos <- posGen
            return (moveTo probe 175 pos, Entity ("moveTo " ++ [probe probeLetter], pos)))
    , (1, do
        wGen <- promote $ do
            old <- readProperty (probeWidth dev)
            return $ suchThat (choose (9, 18)) (/= old)
        return $ do
            w <- wGen
            return (writeProperty (probeWidth dev) w, Entity (propertyName (probeWidth dev), w)))
    ]
