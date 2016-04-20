{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Control.Monad.StaticScope
    ( AccuredDevice
--    , AccureT
--    , ScopeT
    , Impure
    , Flist(..)
--    , accure
    , accures
    , accures'
--    , fromScope
--    , toScope
--    , inScope
--    , scopeAction
    )
where

-- excpetions
import Control.Monad.Catch

-- mtl
import Control.Monad.State.Strict

-- stm
import Control.Concurrent.STM.TVar

-- internal
import Control.Monad.Regions
import Control.Monad.Action.Internal

data Flist (p :: k -> *) (c :: [k]) where
    Nil  :: Flist p '[]
    (:.) :: p a -> Flist p as -> Flist p (a ': as)

infixr 5 :.

-- |
-- > accures (dynHnd0 :. dynHnd1 :. dynHnd2 :. Nil) $
-- >     \(staticHnd0 :. staticHnd0 :. staticHnd0 :. Nil) -> do
-- >         doSomeThingWith staticHnd0
-- >         doSomeThingWith staticHnd1
-- >         doSomeThingWith staticHnd2
accures :: MonadUnderA m => Flist DeviceHandle devs -> (forall s. Flist (AccuredDevice s) devs -> Region s m a) -> m a
accures dynHnds action = region (regionAct dynHnds action)
    where
        regionAct :: MonadUnderA m => Flist DeviceHandle devs -> (Flist (AccuredDevice s) devs -> Region s m a) -> Region s m a
        regionAct d f =
            let (locks, hnds) = go d
            in withDevices locks $ const $ f hnds

        go :: Flist DeviceHandle devs -> ([TVar DeviceAvailability], Flist (AccuredDevice s) devs)
        go Nil           = ([], Nil)
        go (dev :. devs) =
            let (locks, accured) = go devs
            in (uncurry (:) (handleLockPart dev) ++ locks, MkAccuredDevice dev :. accured)

accures' :: (MonadCatch n, InScope m (Region s n), MonadUnderA m) => Flist DeviceHandle devs -> Region s m (Flist (AccuredDevice s) devs)
accures' dynHnds = do
    let (_, hnds) = go dynHnds
    onExit (error "Final" `onException` error "Final2")
    return hnds
    where
        go :: Flist DeviceHandle devs -> ([TVar DeviceAvailability], Flist (AccuredDevice s) devs)
        go Nil           = ([], Nil)
        go (dev :. devs) =
            let (locks, accured) = go devs
            in (uncurry (:) (handleLockPart dev) ++ locks, MkAccuredDevice dev :. accured)
