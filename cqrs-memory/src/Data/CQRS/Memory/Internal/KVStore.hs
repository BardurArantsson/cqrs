{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Memory.Internal.KVStore
    ( newKVStore
    ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.CQRS.KVStore
import qualified Data.Map.Strict as M
import           UnliftIO.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified UnliftIO.Streams.List as SL

newKVStore :: (Ord k, MonadUnliftIO m) => m (KVStore k a)
newKVStore = do
  -- Start with an empty map
  ref <- newIORef M.empty
  -- Convenience functions
  let update f = atomicModifyIORef' ref f
  let update_ f = update (\a -> (f a, ()))
  -- Build the KVStore
  return $ KVStore
    { kvsAdjust = \f k -> update_ $ M.adjust f k
    , kvsAdjustWithKey = \f k -> update_ $ M.adjustWithKey f k
    , kvsAlter = \f k -> update_ $ M.alter f k
    , kvsDelete = \k -> update_ $ M.delete k
    , kvsInsert = \k a -> update_ $ M.insert k a
    , kvsInsertWith = \f k a -> update_ $ M.insertWith f k a
    , kvsLookup = \k -> fmap (M.lookup k) $ readIORef ref
    , kvsTraverse = (=<< (readIORef ref >>= (SL.fromList . M.toList)))
    }
