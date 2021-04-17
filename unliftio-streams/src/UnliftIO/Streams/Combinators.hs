module UnliftIO.Streams.Combinators
  ( filter
  , fold
  , foldM
  , map
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO, liftIO)
import           Prelude hiding (filter, map)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

{-# INLINE filter #-}
filter :: (MonadUnliftIO m) => (a -> Bool) -> InputStream a -> m (InputStream a)
filter f is = liftIO $ SC.filter f is

{-# INLINE fold #-}
fold :: (MonadUnliftIO m) => (s -> a -> s) -> s -> InputStream a -> m s
fold f s0 is = liftIO $ SC.fold f s0 is

{-# INLINE foldM #-}
foldM :: MonadUnliftIO m => (s -> a -> m s) -> s -> InputStream a -> m s
foldM f s0 is =
  withRunInIO $ \io ->
    SC.foldM (\s a -> io $ f s a) s0 is

{-# INLINE map #-}
map :: MonadUnliftIO m => (a -> b) -> InputStream a -> m (InputStream b)
map f is = liftIO $ SC.map f is
