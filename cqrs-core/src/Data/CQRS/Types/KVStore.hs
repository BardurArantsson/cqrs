{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CQRS.Types.KVStore
  ( KVStore(..)
  , transform
  ) where

import           Control.Monad ((>=>))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Data.Bifunctor (bimap)
import           Data.CQRS.Types.Iso
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.Combinators as SC

-- | Key value store for values of type 'a' identified by keys of type
-- 'k'. Each operation is atomic.
data KVStore k a = KVStore {
    -- | Adjust value at the given key. Nothing happens if the value
    -- doesn't exist.
    kvsAdjust :: forall m . (MonadUnliftIO m) => (a -> a) -> k -> m ()

    -- | Adjust value at the given key. Nothing happens if the value
    -- doesn't exist.
  , kvsAdjustWithKey :: forall m . (MonadUnliftIO m) => (k -> a -> a) -> k -> m ()

    -- | Alter the contents of a value, absence or presence of a value
    -- with the given function.
  , kvsAlter :: forall m . (MonadUnliftIO m) => (Maybe a -> Maybe a) -> k -> m ()

    -- | Delete value at the given key. Nothing happens if the value
    -- doesn't exist.
  , kvsDelete :: forall m . (MonadUnliftIO m) => k -> m ()

    -- | Insert a value at the given key. Any existing value will be
    -- replaced. Equivalent to 'kvsInsertWith const'.
  , kvsInsert :: forall m . (MonadUnliftIO m) => k -> a -> m ()

    -- | Insert a value at the given key, using the given function to
    -- combine any existing value with the value. If the value does
    -- not exist already, the given value is inserted without
    -- modification.
  , kvsInsertWith :: forall m . (MonadUnliftIO m) => (a -> a -> a) -> k -> a -> m ()

    -- | Look up value at the given key.
  , kvsLookup :: forall m . (MonadUnliftIO m) => k -> m (Maybe a)

  , -- | Traverse all key-value pairs.
    kvsTraverse :: forall m b . (MonadUnliftIO m) => (InputStream (k, a) -> m b) -> m b

  }

-- | Transform an implementation of 'KVStore i a' to an implementation
-- of 'KVStore j b' via two isomorphisms.
transform :: forall a' a k' k . Iso k' k -> Iso a' a -> KVStore k a -> KVStore k' a'
transform (fk, gk) (fa, ga) KVStore{..} =
    KVStore
      { kvsAdjust = kvsAdjust'
      , kvsAdjustWithKey = kvsAdjustWithKey'
      , kvsAlter = kvsAlter'
      , kvsDelete = kvsDelete'
      , kvsInsert = kvsInsert'
      , kvsInsertWith = kvsInsertWith'
      , kvsLookup = kvsLookup'
      , kvsTraverse = kvsTraverse'
      }
  where

    kvsAdjust' :: forall m' . (MonadUnliftIO m') => (a' -> a') -> k' -> m' ()
    kvsAdjust' f' k' = kvsAdjust (fa . f' . ga) $ fk k'

    kvsAdjustWithKey' :: forall m' . (MonadUnliftIO m') => (k' -> a' -> a') -> k' -> m' ()
    kvsAdjustWithKey' f' k' = kvsAdjustWithKey (\k -> fa . f' (gk k) . ga) (fk k')

    kvsAlter' :: forall m' . (MonadUnliftIO m') => (Maybe a' -> Maybe a') -> k' -> m' ()
    kvsAlter' f' k' = kvsAlter (fmap fa . f' . fmap ga) (fk k')

    kvsDelete' :: forall m' . (MonadUnliftIO m') => k' -> m' ()
    kvsDelete' k' = kvsDelete $ fk k'

    kvsInsert' :: forall m' . (MonadUnliftIO m') => k' -> a' -> m' ()
    kvsInsert' k' a' = kvsInsert (fk k') (fa a')

    kvsInsertWith' :: forall m' . (MonadUnliftIO m') => (a' -> a' -> a') -> k' -> a' -> m' ()
    kvsInsertWith' f' k' a' = kvsInsertWith (\a0 a1 -> fa $ f' (ga a0) (ga a1)) (fk k') (fa a')

    kvsLookup' :: forall m' . (MonadUnliftIO m') => k' -> m' (Maybe a')
    kvsLookup' k' = fmap (fmap ga) (kvsLookup (fk k'))

    kvsTraverse' :: forall m' b . (MonadUnliftIO m') => (InputStream (k', a') -> m' b) -> m' b
    kvsTraverse' f' = kvsTraverse $ SC.map (bimap gk ga) >=> f'
