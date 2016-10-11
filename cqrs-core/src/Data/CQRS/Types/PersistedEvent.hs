{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Data.CQRS.Types.PersistedEvent
       ( PersistedEvent(..)
       ) where

import Control.DeepSeq (NFData(..))
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic)

-- | Persisted Event.
data PersistedEvent i e =
  PersistedEvent { peEvent :: !e            -- ^ Event.
                 , peSequenceNumber :: !Int -- ^ Sequence number within the aggregate.
                 , peAggregateId :: !i      -- ^ Identifier of aggregate that event applies to.
                 }
  deriving (Show, Eq, Generic)

instance Bifunctor PersistedEvent where
  bimap f g (PersistedEvent e seqNo i) = PersistedEvent (g e) seqNo (f i)

instance (NFData e, NFData i) => NFData (PersistedEvent i e)
