module Data.CQRS.Memory.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import           Control.Monad (liftM)
import           Data.CQRS.Types.Snapshot
import           Data.CQRS.Types.SnapshotStore
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type SnapStore i a = TVar (Map i (Snapshot a))

writeSnapshot :: (Ord i) => SnapStore i a -> i -> Snapshot a -> IO ()
writeSnapshot store aggregateId snapshot = do
  atomically $ modifyTVar' store (M.insert aggregateId snapshot)

readSnapshot :: (Ord i) => SnapStore i a -> i -> IO (Maybe (Snapshot a))
readSnapshot store aggregateId = do
  atomically $ liftM (M.lookup aggregateId) $ readTVar store

-- | Create a new memory-backed snapshot store.
newSnapshotStore :: (Ord i) => IO (SnapshotStore i a)
newSnapshotStore = do
  store <- atomically $ newTVar M.empty
  return $ SnapshotStore
             (writeSnapshot store)
             (readSnapshot store)
