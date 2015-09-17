{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Data.CQRS.Types.PersistedEvent
       ( PersistedEvent(..)
       ) where

import Control.DeepSeq (NFData(..))
import Data.UUID.Types (UUID)
import GHC.Generics (Generic)

-- | Persisted Event.
data PersistedEvent e =
  PersistedEvent { peEvent :: !e            -- ^ Event.
                 , peSequenceNumber :: !Int -- ^ Sequence number within the aggregate.
                 , peEventId :: !UUID       -- ^ UUID for the event itself.
                 }
  deriving (Show, Eq, Functor, Generic)

instance NFData e => NFData (PersistedEvent e)
