module System.Hardware.Gilson.OrbitalShaker where

-- base
import Control.Monad

import Data.Functor.Identity

import Data.Typeable
import Data.IORef
import Data.Word

-- excpetions
import Control.Monad.Catch

-- stm
import Control.Concurrent.STM.TVar

-- transformers
import Control.Monad.IO.Class

-- internal
import Control.Monad.Action
import Control.Monad.Statemachine

import System.Hardware.GSIOC

data OrbitalShaker u = OrbitalShaker
    { osCurrentSpeed :: StateM OrbitalShaker GSIOC u Identity Word16 }
    deriving Typeable

mkOrbitalShaker :: MonadUnderA m => Action OrbitalShaker GSIOC Create m (OrbitalShaker ())
mkOrbitalShaker = do
    val <- request
    var <- liftIO $ newTVarIO False
    st1 <- newStateM (simpleStatusRequest request) (return Nothing) (simpleMut mutator) (sharedWaiter var simpleWait) (Just val)
    return (OrbitalShaker st1)
    where
        request :: (MonadUnderA m, Typeable dev) => Action dev GSIOC eff m (Identity Word16)
        request = do
            c <- immediateEnum 'R' [("T", True), ("F", False)]
            if not c
                then return $ pure 0
                else immediate 'S' >>= return . pure . read

        mutator v
            | v == 0             = buffered "RF"
            | v < 20 || v > 720 = invalidTarger $ show v ++ " not 0 or [20..720]"
            | otherwise         = do
                buffered $ "S" ++ show v
                buffered "RT"
