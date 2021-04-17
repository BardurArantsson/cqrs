{-# LANGUAGE LambdaCase #-}
module Data.Foleni.Internal
  ( FQueue(..)
  , newFQueue
  , readFQueue
  , writeFQueue
  ) where

import           Control.Concurrent.STM (STM)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue, isEmptyTQueue)

-- | Queue containing elements of type 'a'.
data FQueue a = FQueue (TQueue a) (a -> Int) (TVar Int) !Int

-- | Create a queue with the given size bound and cost function.
newFQueue :: Int -> (a -> Int) -> STM (FQueue a)
newFQueue maxSize estimateSize = do
  q <- newTQueue
  currentSize <- newTVar 0
  pure $ FQueue q estimateSize currentSize maxSize

-- | Read a single element. Blocks until an element is available.
readFQueue :: FQueue a -> STM a
readFQueue (FQueue q _ _ _) = readTQueue q

-- | Write a single element if there is enough estimated free space.
-- Returns 'True' if the element was written, 'False' otherwise.
--
-- __Note:__ If the queue is empty then the element will always be
-- accepted regardless of size. This means progress can always be made
-- in producer-consumer scenarios.
writeFQueue :: FQueue a -> a -> STM Bool
writeFQueue (FQueue q estimateSize currentSize maxSize) a = do
  -- Write element to queue and account for it.
  let write = do
        writeTQueue q a
        modifyTVar' currentSize ((+) n)
        pure True
  -- Empty queue means we always write the element. The element
  -- is already taking up memory, and it would otherwise be too
  -- easy to accidentally end up rejecting *every* element if
  -- the maxSize is accidentally too low. (XXX: Does this rationale hold up?)
  isEmptyTQueue q >>= \case
    True -> do
      write
    False -> do
      n' <- readTVar currentSize
      if (n + n' <= maxSize) then
          write
        else
          pure False
  where
    -- Size of the element
    n = estimateSize a
