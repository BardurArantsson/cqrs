module Data.CQRS.Memory.Internal.Storage
    ( Event(..)
    , Store(..)
    , Storage(..)
    , newStorage
    ) where

import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.Int (Int64)
import           Data.IORef (IORef, newIORef)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S

data Event i e =
    Event { eAggregateId :: i
          , ePersistedEvent :: PersistedEvent e
          , eTimestamp :: Int64
          }
    deriving (Show)

data Store i e = Store
    { msEvents :: Seq (Event i e)
      -- Current global time stamp. Starts at 1.
    , msCurrentTimestamp :: Int64
    }

-- | Storage used for memory-backed EventStore.
newtype Storage i e = Storage (IORef (Store i e))

-- | Create backing memory for a memory-based event store
-- or archive store.
newStorage :: IO (Storage i e)
newStorage = fmap Storage $ newIORef $ Store S.empty 1
