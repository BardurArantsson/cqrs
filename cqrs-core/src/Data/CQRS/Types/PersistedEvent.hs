{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Data.CQRS.Types.PersistedEvent
       ( PersistedEvent(..)
       ) where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

-- | Persisted Event.
data PersistedEvent e =
  PersistedEvent { peEvent :: !e            -- ^ Event.
                 , peSequenceNumber :: !Int -- ^ Sequence number within the aggregate.
                 }
  deriving (Show, Eq, Functor, Generic)

instance NFData e => NFData (PersistedEvent e)
