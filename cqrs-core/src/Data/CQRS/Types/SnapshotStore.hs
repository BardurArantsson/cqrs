{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.SnapshotStore
    ( SnapshotStore(..)
    , transform
    , nullSnapshotStore
    ) where

import Control.Applicative ((<$>))
import Data.CQRS.Types.Snapshot (Snapshot(..))
import Data.CQRS.Types.Iso

-- | A snapshot store is used for storing snapshots.
data SnapshotStore i a = SnapshotStore
    {
      -- | Write out a snapshot. Snapshot version numbers are NOT
      -- checked for validity (e.g. whether they are greater than the
      -- existing snapshot version numbers).
      ssWriteSnapshot :: i -> Snapshot a -> IO ()
    ,
      -- | Read latest snapshot of an aggregate. Snapshot stores are
      -- permitted to return 'Nothing' in all cases.
      ssReadSnapshot :: i -> IO (Maybe (Snapshot a))
    }

-- | Transform an implementation of 'SnapshotStore i a' to an
-- implementation of 'SnapshotStore j b' via a prism (for the
-- snapshot) and isomorphism (for the aggregate ID). This is typically
-- used to add serialization/deserialization to snapshot stores which
-- do not support storing anything other than binary data. Note that
-- the prism is allowed to return 'Nothing' to indicate that data
-- could not be transformed. This can be used to avoid the need for
-- complicated versioning of snapshot data by using simple version
-- tags/hashes to determine compatibility with stored snapshots.
transform :: forall a a' i i' . (a' -> a, a -> Maybe a') -> Iso i' i -> SnapshotStore i a -> SnapshotStore i' a'
transform (fa, ga) (fi, _) (SnapshotStore writeSnapshot' readSnapshot') =
  SnapshotStore writeSnapshot readSnapshot
    where
      writeSnapshot :: i' -> Snapshot a' -> IO ()
      writeSnapshot aggregateId (Snapshot v a) =
        writeSnapshot' (fi aggregateId) $ Snapshot v $ fa a

      readSnapshot :: i' -> IO (Maybe (Snapshot a'))
      readSnapshot aggregateId =
        (f' =<<) <$> readSnapshot' (fi aggregateId)
          where
            f' (Snapshot v a) = ga a >>= Just . Snapshot v

-- | The "null" snapshot store, i.e. a snapshot store which never
-- actually stores any snapshots.
nullSnapshotStore :: SnapshotStore i a
nullSnapshotStore =
    SnapshotStore { ssWriteSnapshot = \_ _ -> return ()
                  , ssReadSnapshot = \_ -> return Nothing
                  }
