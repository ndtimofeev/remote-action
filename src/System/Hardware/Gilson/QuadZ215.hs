module System.Hardware.Gilson.QuadZ215 where

-- base
import Data.Typeable

-- internal
import Control.Monad.Action
import Control.Monad.Statemachine

import System.Hardware.GSIOC

data QuadZ215 s = QuadZ215
    { qzHandPosition :: StateM QuadZ215 GSIOC s Maybe (Integer, Integer) }
    deriving Typeable
