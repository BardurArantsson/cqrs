{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.CQRS.Test.Internal.EventStoreTest
    ( mkEventStoreSpec
    ) where

import           Control.Exception.Lifted (try)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, ask, randomUUID)
import qualified Data.CQRS.Test.Internal.Scope as S
import           Data.CQRS.Types.EventStore (EventStore, StoreError(VersionConflict))
import qualified Data.CQRS.Types.EventStore as ES
import           Data.CQRS.Types.PersistedEvent
import           Data.UUID.Types (UUID)
import           Test.Hspec (Spec, describe, shouldBe)
import qualified Test.Hspec as Hspec
import qualified System.IO.Streams.List as SL

-- Ambient data for test scope for each spec.
data Scope e = Scope { scopeEventStore :: EventStore e
                     }

-- Store given events in exactly the order given.
storeEvents :: UUID -> [PersistedEvent e] -> ScopeM (Scope e) ()
storeEvents aggregateId events = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ (ES.esStoreEvents eventStore) aggregateId events

-- Read all events for a given aggregate.
readEvents :: UUID -> ScopeM (Scope e) [PersistedEvent e]
readEvents aggregateId = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ ES.esRetrieveEvents eventStore aggregateId (-1) $ SL.toList

-- Test suite for event store which stores 'ByteString' events.
mkEventStoreSpec :: TestKitSettings a (EventStore ByteString) -> Spec
mkEventStoreSpec testKitSettings = do

  describe "EventStore implementation" $ do

    it "should be able to retrieve stored events" $ do
      aggregateId <- randomUUID
      eventId0 <- randomUUID
      eventId1 <- randomUUID
      -- Write two events.
      let expectedEvents = [ PersistedEvent "test event 0" 0 eventId0
                           , PersistedEvent "test event 1" 1 eventId1
                           ]
      storeEvents aggregateId expectedEvents
      -- Retrieve the stored events.
      actualEvents <- readEvents aggregateId
      -- Assert that we've retrieved the expected events in order.
      verify $ actualEvents `shouldBe` expectedEvents

    it "should throw a VersionConflict exception when storing conflicting events in a single operation" $ do
      aggregateId <- randomUUID
      eventId0 <- randomUUID
      eventId1 <- randomUUID
      -- Write two conflicting events.
      let conflictingEvents = [ PersistedEvent "test event 0" 0 eventId0
                              , PersistedEvent "test event 1" 0 eventId1
                              ]
      storeEvents aggregateId conflictingEvents `shouldThrow` VersionConflict aggregateId
      -- Make sure we didn't actually store any events
      storedEvents <- readEvents aggregateId
      verify $ length storedEvents `shouldBe` 0

    it "should throw a VersionConflict exception when storing conflicting events in multiple operations" $ do
      aggregateId <- randomUUID
      eventId0 <- randomUUID
      eventId1 <- randomUUID
      -- Write a single event
      let initialEvent = PersistedEvent "test event 0" 0 eventId0
      storeEvents aggregateId [initialEvent]
      -- Write the event that should conflict
      let conflictingEvents = [ PersistedEvent "test event 1" 0 eventId1 ]
      storeEvents aggregateId conflictingEvents `shouldThrow` VersionConflict aggregateId
      -- Make sure we didn't write the second event
      storedEvents <- readEvents aggregateId
      verify $ length storedEvents `shouldBe` 1
      verify $ (storedEvents !! 0) `shouldBe` initialEvent

  where
    runScope = S.mkRunScope testKitSettings $ \a -> do
                                     eventStore <- tksMakeContext testKitSettings a
                                     return $ Scope eventStore
    -- Shorthands
    it msg scope = Hspec.it msg $ runScope scope

    shouldThrow action exc = do
      resultOrExc <- try $ action
      liftIO $ resultOrExc `shouldBe` (Left exc)
