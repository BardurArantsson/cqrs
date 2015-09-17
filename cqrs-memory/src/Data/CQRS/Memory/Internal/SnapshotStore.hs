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
import           Data.UUID.Types (UUID)

type SnapStore a = TVar (Map UUID (Snapshot a))

writeSnapshot :: SnapStore a -> UUID -> Snapshot a -> IO ()
writeSnapshot store aggregateId snapshot = do
  atomically $ modifyTVar' store (M.insert aggregateId snapshot)

readSnapshot :: SnapStore a -> UUID -> IO (Maybe (Snapshot a))
readSnapshot store aggregateId = do
  atomically $ liftM (M.lookup aggregateId) $ readTVar store

-- | Create a new memory-backed snapshot store.
newSnapshotStore :: IO (SnapshotStore a)
newSnapshotStore = do
  store <- atomically $ newTVar M.empty
  return $ SnapshotStore
             (writeSnapshot store)
             (readSnapshot store)
