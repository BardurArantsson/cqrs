module Data.CQRS.Test.Internal.AggregateAction
    ( byteStringAggregateAction )
    where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.CQRS.Types.AggregateAction

byteStringAggregateAction :: AggregateAction ByteString ByteString
byteStringAggregateAction maybeA e = maybe e (\a -> B.append a e) maybeA
