module Data.CQRS.Types.SnapshotStore
    ( SnapshotStore(..)
    , applyPrism
    , nullSnapshotStore
    ) where

import Data.CQRS.Types.Snapshot (Snapshot(..))
import Data.UUID.Types (UUID)

-- | A snapshot store is used for storing snapshots.
data SnapshotStore a = SnapshotStore
    {
      -- | Write out a snapshot. Snapshot version numbers are NOT
      -- checked for validity (e.g. whether they are greater than the
      -- existing snapshot version numbers).
      ssWriteSnapshot :: UUID -> Snapshot a -> IO ()
    ,
      -- | Read latest snapshot of an aggregate identified by UUID. A
      -- snapshot store is permitted to return 'Nothing' in all cases.
      ssReadSnapshot :: UUID -> IO (Maybe (Snapshot a))
    }

-- | Transform an implementation of 'SnapshotStore a' to an
-- implementation of 'SnapshotStore b' via a prism. This is typically
-- used to add serialization/deserialization to snapshot stores which
-- do not support storing anything other than binary data. Note that
-- the prism is allowed to return 'Nothing' to indicate that data could
-- not be transformed. This can be used to avoid the need for complicated
-- versioning of snapshot data by using simple version tags/hashes to
-- determine compatibility with stored snapshots.
applyPrism :: (a -> b, b -> Maybe a) -> SnapshotStore b -> SnapshotStore a
applyPrism (f, g) (SnapshotStore writeSnapshot' readSnapshot') =
    SnapshotStore writeSnapshot readSnapshot
    where
      writeSnapshot aggregateId (Snapshot v a) = do
          writeSnapshot' aggregateId $ Snapshot v $ f a
      readSnapshot aggregateId = do
          fmap (f' =<<) $ (readSnapshot' aggregateId)
          where
            f' (Snapshot v a) = g a >>= Just . Snapshot v

-- | The "null" snapshot store, i.e. a snapshot store which never
-- actually stores any snapshots.
nullSnapshotStore :: SnapshotStore a
nullSnapshotStore =
    SnapshotStore { ssWriteSnapshot = \_ _ -> return ()
                  , ssReadSnapshot = \_ -> return Nothing
                  }
