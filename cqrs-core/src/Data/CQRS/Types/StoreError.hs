module Data.CQRS.Types.StoreError
    ( StoreError(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.UUID.Types (UUID)

-- | Errors that can happen during 'esStoreEvents'.
data StoreError = VersionConflict UUID
  deriving (Typeable, Show, Eq)

instance Exception StoreError
