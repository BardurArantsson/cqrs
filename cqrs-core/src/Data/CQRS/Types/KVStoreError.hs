module Data.CQRS.Types.KVStoreError
    ( KVStoreError(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | Errors that can happen while operating on a KVStore.
data KVStoreError k = UnreadableValue k String
  deriving (Show, Eq)

instance (Typeable i, Show i) => Exception (KVStoreError i)
