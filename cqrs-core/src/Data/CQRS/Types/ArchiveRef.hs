module Data.CQRS.Types.ArchiveRef
       ( ArchiveRef(..)
       ) where

import Data.Typeable (Typeable)
import Data.UUID.Types (UUID)

-- | Archive reference.
data ArchiveRef = CurrentArchive | NamedArchive UUID
  deriving (Typeable, Show, Eq)
