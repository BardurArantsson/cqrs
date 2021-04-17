module UnliftIO.Streams.List
  ( chunkList
  , fromList
  , toList
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Prelude hiding (map)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.List as SL

{-# INLINE chunkList #-}
chunkList :: (MonadUnliftIO m) => Int -> InputStream a -> m (InputStream [a])
chunkList n is = liftIO $ SL.chunkList n is

{-# INLINE toList #-}
toList :: (MonadUnliftIO m) => InputStream a -> m [a]
toList = liftIO . SL.toList

{-# INLINE fromList #-}
fromList :: (MonadUnliftIO m) => [a] -> m (InputStream a)
fromList = liftIO . SL.fromList
