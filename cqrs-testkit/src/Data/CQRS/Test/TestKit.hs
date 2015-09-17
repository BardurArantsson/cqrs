module Data.CQRS.Test.TestKit
    ( TestKitSettings(..)
    , mkEventStoreSpec
    , mkRepositorySpec
    , mkSnapshotStoreSpec
    , mkArchiveStoreSpec
    ) where

import Data.CQRS.Test.Internal.ArchiveStoreTest (mkArchiveStoreSpec)
import Data.CQRS.Test.Internal.EventStoreTest (mkEventStoreSpec)
import Data.CQRS.Test.Internal.RepositoryTest (mkRepositorySpec)
import Data.CQRS.Test.Internal.SnapshotTest (mkSnapshotStoreSpec)
import Data.CQRS.Test.Internal.TestKitSettings (TestKitSettings(..))
