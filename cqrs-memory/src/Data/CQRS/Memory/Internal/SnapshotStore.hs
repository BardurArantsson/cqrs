module Data.CQRS.Memory.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.CQRS.Types.Snapshot
import           Data.CQRS.Types.SnapshotStore
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           UnliftIO.STM (atomically)

type SnapStore i a = TVar (Map i (Snapshot a))

writeSnapshot :: (Ord i, MonadUnliftIO m) => SnapStore i a -> i -> Snapshot a -> m ()
writeSnapshot store aggregateId snapshot =
  atomically $ modifyTVar' store (M.insert aggregateId snapshot)

readSnapshot :: (Ord i, MonadUnliftIO m) => SnapStore i a -> i -> m (Maybe (Snapshot a))
readSnapshot store aggregateId =
  atomically $ M.lookup aggregateId <$> readTVar store

-- | Create a new memory-backed snapshot store.
newSnapshotStore :: (Ord i) => IO (SnapshotStore i a)
newSnapshotStore = do
  store <- atomically $ newTVar M.empty
  return $ SnapshotStore
             (writeSnapshot store)
             (readSnapshot store)
