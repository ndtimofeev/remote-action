module Control.Monad.Action (
    -- * Device representation
    DeviceHandle,
    AccuredDevice,
    Proto,
    -- * Effect representation
    Pure,
    Impure,
    Create,
    IfImpure,
    ifImpure,
    ifImpureM,
    Ways(..),
    -- * Evaluation representation
    Action,
    MonadUnderA,
    withDevice,
    protocol,
    localProtocol,
    getTypeRep,
    pureAsync,
    -- * Constructor representation
    Nominal,
    addToStage,
    CreateAction,
    CreateCtx,
    Raw,
    New,
    mkDevice,
    mkSubDevice,
    -- * Exceptions
    deviceError,
    DeviceRuntimeError,
) where

-- internal
import Control.Monad.Action.Internal
