module Data.CQRS.Types.ArchiveMetadata
    ( ArchiveMetadata(..)
    ) where

import Data.CQRS.Types.ArchiveRef (ArchiveRef)
import Data.Typeable (Typeable)
import Data.UUID.Types (UUID)

-- | Archive metadata.
data ArchiveMetadata = ArchiveMetadata
    { amArchiveId :: UUID
    , amPreviousArchiveId :: Maybe UUID
    , amNextArchiveId :: ArchiveRef
    } deriving (Typeable, Show, Eq)
