{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Types.SnapshotStore
    ( SnapshotStore(..)
    , transform
    , transformI
    , transformA
    , nullSnapshotStore
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.Iso

-- | A snapshot store is used for storing snapshots.
data SnapshotStore i a = SnapshotStore
    {
      -- | Write out a snapshot. Snapshot version numbers are NOT
      -- checked for validity (e.g. whether they are greater than the
      -- existing snapshot version numbers).
      ssWriteSnapshot :: forall m . MonadUnliftIO m => i -> Snapshot a -> m ()
    ,
      -- | Read latest snapshot of an aggregate. Snapshot stores are
      -- permitted to return 'Nothing' in all cases.
      ssReadSnapshot :: forall m . MonadUnliftIO m => i -> m (Maybe (Snapshot a))
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
transform afg ifg snapshotStore =
  transformA afg $ transformI ifg snapshotStore

-- | Transform a 'SnapshotStore i a' to a 'SnapshotStore j a' using
-- and isomorphism between 'i' and 'j'.
transformI :: forall a i i' . Iso i' i -> SnapshotStore i a -> SnapshotStore i' a
transformI (fi, _) (SnapshotStore writeSnapshot' readSnapshot') =
  SnapshotStore writeSnapshot readSnapshot
    where
      writeSnapshot :: forall m' . MonadUnliftIO m' => i' -> Snapshot a -> m' ()
      writeSnapshot aggregateId snapshot =
        writeSnapshot' (fi aggregateId) snapshot

      readSnapshot :: forall m' . MonadUnliftIO m' => i' -> m' (Maybe (Snapshot a))
      readSnapshot aggregateId =
        readSnapshot' (fi aggregateId)

-- | Transform a 'SnapshotStore i a' to a 'SnapshotStore i b' using
-- and isomorphism between 'a' and 'b'.
transformA :: forall a a' i . (a' -> a, a -> Maybe a') -> SnapshotStore i a -> SnapshotStore i a'
transformA (fa, ga) (SnapshotStore writeSnapshot' readSnapshot') =
  SnapshotStore writeSnapshot readSnapshot
    where
      writeSnapshot :: forall m' . MonadUnliftIO m' => i -> Snapshot a' -> m' ()
      writeSnapshot aggregateId (Snapshot v a) =
        writeSnapshot' aggregateId $ Snapshot v $ fa a

      readSnapshot :: forall m' . MonadUnliftIO m' => i -> m' (Maybe (Snapshot a'))
      readSnapshot aggregateId =
        (f' =<<) <$> readSnapshot' aggregateId
          where
            f' (Snapshot v a) = ga a >>= Just . Snapshot v

-- | The "null" snapshot store, i.e. a snapshot store which never
-- actually stores any snapshots.
nullSnapshotStore :: SnapshotStore i a
nullSnapshotStore =
    SnapshotStore { ssWriteSnapshot = \_ _ -> return ()
                  , ssReadSnapshot = \_ -> return Nothing
                  }
