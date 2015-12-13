module Data.CQRS.Types.StoreError
    ( StoreError(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | Errors that can happen during 'esStoreEvents'.
data StoreError i = VersionConflict i
  deriving (Typeable, Show, Eq)

instance (Typeable i, Show i, Eq i) => Exception (StoreError i)
