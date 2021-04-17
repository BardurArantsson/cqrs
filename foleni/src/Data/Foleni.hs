{-# LANGUAGE LambdaCase #-}
module Data.Foleni
    ( Config
    , Input(..)
    , Output(..)
    , bounded
    , boundedBy
    , newQueue
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM as S
import           Control.Monad (void)
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import           Prelude hiding (read)
import           Data.Foleni.Internal

-- | A source for values. Returns 'Nothing' when the source is sealed.
data Input a =
  Input { recv :: STM (Maybe a) }

-- | A sink for values. Returns 'Left ()' when the queue has been sealed
-- and cannot accept any further values.
--
-- Returns 'Right added' where 'added' is 'True' iff the value was added
-- to the queue and 'False' if the queue was full.
data Output a =
  Output { send :: a -> STM (Either () Bool) }

-- | Configuration for the queue.
data Config a = BoundedBy Int (a -> Int)

-- | Create a configuration for a queue bounded by the given 'maxCost'
-- where the given 'cost' function is used to calculate the cost of
-- any given element.
boundedBy :: Int -> (a -> Int) -> Config a
boundedBy maxCost cost = BoundedBy maxCost cost

-- | Create a configuration for a queue bounded by the given number of
-- elements. Calling 'bounded n' is equivalent to 'boundedBy n (const
-- 1)'.
bounded :: Int -> Config a
bounded n = boundedBy n (const 1)

-- | Create a new queue with the given configuration. The function
-- returns 'o i seal' where 'o' is an 'Output a', 'i' is an 'Input a',
-- and 'seal' is a function that when called will seal the channel.
newQueue :: MonadUnliftIO m => Config a -> m (Output a, Input a, STM ())
newQueue (BoundedBy maxSize estimateSize) = liftIO $ do
  -- This is heavily inspired by Pipes.Concurrent.spawn'.
  (write, read) <- do
    q <- atomically $ newFQueue maxSize estimateSize
    return (writeFQueue q, readFQueue q)

  -- Sealing flag
  sealed <- S.newTVarIO False
  let seal = S.writeTVar sealed True

  -- Use weak TVars to keep track of whether the 'Input' or 'Output'
  -- has been garbage collected.  Seal the mailbox when either of them
  -- becomes garbage collected.
  rSend <- S.newTVarIO ()
  void $ S.mkWeakTVar rSend (S.atomically seal)
  rRecv <- S.newTVarIO ()
  void $ S.mkWeakTVar rRecv (S.atomically seal)

  -- Build the send/recv functions
  let sendOrEnd a = do
        b <- S.readTVar sealed
        if b
          then return $ Left ()
          else do
            fmap Right $ write a

      readOrEnd = (Just <$> read) <|> (do
        b <- S.readTVar sealed
        S.check b
        return Nothing )

      _send a = sendOrEnd a <* S.readTVar rSend
      _recv   = readOrEnd   <* S.readTVar rRecv

  return (Output _send, Input _recv, seal)
