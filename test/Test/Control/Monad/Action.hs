module Test.Control.Monad.Action where

-- base
import Control.Applicative
import Control.Monad

import Data.Function
import Data.IORef
import Data.Typeable

-- exceptions
import Control.Monad.Catch

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

-- QuickCheck
import Test.QuickCheck.Gen

-- internal
import Control.Monad.Action
import Control.Monad.Injectable

data LogEntity = forall a. (Eq a, Show a, Typeable a) => Entity a

instance Eq LogEntity where
    Entity v1 == Entity v2 = maybe False (==v2) (cast v1)

deriving instance Show LogEntity

runTest :: (MonadIO m, MonadCatch m) => Gen (m (m (), LogEntity)) -> Int -> m (Either [(m (), LogEntity)] ())
runTest generator series = do
    ref <- liftIO $ newIORef []
    res <- try $ replicateM_ series $ do
        testGen                   <- liftIO $ generate generator
        val@(test, Entity entity) <- testGen
        liftIO $ do
            modifyIORef' ref (val :)
            print entity
        test
    case res of
        Right _                  -> return $ Right ()
        Left (SomeException err) -> liftIO $ do
            putStrLn $ displayException err
            h    <- readIORef ref
            return $ Left $ reverse h

reproduce :: (MonadIO m, MonadCatch m) => [(m (), LogEntity)] -> m () -> m (Maybe [(m (), LogEntity)])
reproduce xs recovery = case xs of
    []       -> return Nothing
    (_ : vs) -> do
        recovery
        let eval (act, Entity ent) = do
                liftIO $ print ent
                act
        res <- try $ forM_ (init xs) eval
        case res of
            Left (SomeException err) -> do
                liftIO $ putStrLn $ displayException err
                return Nothing
            Right _ -> do
                res' <- try $ eval $ last xs
                case res' of
                    Right _                  -> return Nothing
                    Left (SomeException err) -> do
                        liftIO $ print $ displayException err
                        (<|> Just xs) <$> reproduce vs recovery
