{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.CQRS.Internal.UState
  ( UState
  , UStateT
  , runUStateT
  , modifyU
  , getU
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import           Data.Functor.Identity
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef')

type UState s = UStateT s Identity

newtype UStateT s m a = UStateT { unUStateT :: ReaderT (IORef s) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (UStateT s) where
  lift m = UStateT $ lift m

instance MonadIO m => MonadIO (UStateT s m) where
  liftIO m = UStateT $ liftIO m

instance MonadUnliftIO m => MonadUnliftIO (UStateT s m) where
  withRunInIO f = UStateT $ ReaderT $ \r ->
    withRunInIO $ \io ->
      f $ io . flip (runReaderT . unUStateT) r

-- | Run a computation with the given environment and return its
-- output and the resulting environment.
runUStateT :: (MonadUnliftIO m) => UStateT s m a -> s -> m (a, s)
runUStateT (UStateT run) s = do
  ref <- liftIO $ newIORef s
  a <- runReaderT run ref
  s' <- liftIO $ readIORef ref
  return (a, s')

-- | Update state using the given function.
modifyU :: (MonadUnliftIO m) => (s -> s) -> UStateT s m ()
modifyU f = UStateT $ do
  ref <- ask
  liftIO $ modifyIORef' ref f

-- | Get state.
getU :: (MonadUnliftIO m) => UStateT s m s
getU = UStateT $ do
  ref <- ask
  liftIO $ readIORef ref
