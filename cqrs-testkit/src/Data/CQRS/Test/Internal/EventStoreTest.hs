{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.Test.Internal.EventStoreTest
    ( mkEventStoreSpec
    ) where

import           Control.Exception.Lifted (try)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, ask)
import qualified Data.CQRS.Test.Internal.Scope as S
import           Data.CQRS.Test.Internal.Utils (randomId)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.EventStore (EventStore, StoreError(VersionConflict))
import qualified Data.CQRS.Types.EventStore as ES
import           Data.CQRS.Types.PersistedEvent
import           Test.Hspec (Spec, describe, shouldBe)
import qualified Test.Hspec as Hspec
import qualified UnliftIO.Streams.List as SL

-- Ambient data for test scope for each spec.
newtype Scope i e = Scope { scopeEventStore :: EventStore i e
                          }

-- Store given events in exactly the order given.
storeEvents :: i -> [PersistedEvent e] -> ScopeM (Scope i e) ()
storeEvents aggregateId events = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ forM_ (C.fromList aggregateId events) $ ES.esStoreEvents eventStore

-- Read all events for a given aggregate.
readEvents :: i -> ScopeM (Scope i e) [PersistedEvent e]
readEvents aggregateId = do
  eventStore <- fmap scopeEventStore ask
  ES.esRetrieveEvents eventStore aggregateId (-1) SL.toList

-- Test suite for event store which stores 'ByteString' events.
mkEventStoreSpec :: TestKitSettings a (EventStore ByteString ByteString) -> Spec
mkEventStoreSpec testKitSettings =

  describe "EventStore implementation" $ do

    it "should be able to retrieve stored events" $ do
      aggregateId <- randomId
      -- Write two events.
      let expectedEvents = [ PersistedEvent "test event 0" 0 55
                           , PersistedEvent "test event 1" 1 56
                           ]
      storeEvents aggregateId expectedEvents
      -- Retrieve the stored events.
      actualEvents <- readEvents aggregateId
      -- Assert that we've retrieved the expected events in order.
      verify $ actualEvents `shouldBe` expectedEvents

    it "should throw a VersionConflict exception when storing conflicting events in a single operation" $ do
      aggregateId <- randomId
      -- Write two conflicting events.
      let conflictingEvents = [ PersistedEvent "test event 0" 0 99
                              , PersistedEvent "test event 1" 0 100
                              ]
      storeEvents aggregateId conflictingEvents `shouldThrow` VersionConflict aggregateId
      -- Make sure we didn't actually store any events
      storedEvents <- readEvents aggregateId
      verify $ length storedEvents `shouldBe` 0

    it "should throw a VersionConflict exception when storing conflicting events in multiple operations" $ do
      aggregateId <- randomId
      -- Write a single event
      let initialEvent = PersistedEvent "test event 0" 0 32
      storeEvents aggregateId [initialEvent]
      -- Write the event that should conflict
      let conflictingEvents = [ PersistedEvent "test event 1" 0 33]
      storeEvents aggregateId conflictingEvents `shouldThrow` VersionConflict aggregateId
      -- Make sure we didn't write the second event
      storedEvents <- readEvents aggregateId
      verify $ length storedEvents `shouldBe` 1
      verify $ head storedEvents `shouldBe` initialEvent

  where
    runScope = S.mkRunScope testKitSettings $ \a -> do
                                     eventStore <- tksMakeContext testKitSettings a
                                     return $ Scope eventStore
    -- Shorthands
    it msg scope = Hspec.it msg $ runScope scope

    shouldThrow action exc = do
      resultOrExc <- try action
      liftIO $ resultOrExc `shouldBe` Left exc
