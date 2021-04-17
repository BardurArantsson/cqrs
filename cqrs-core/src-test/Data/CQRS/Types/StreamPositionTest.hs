{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.CQRS.Types.StreamPositionTest
  ( streamPositionSpec
  ) where

import           Data.CQRS.Internal.StreamPosition
import           Data.GenValidity (GenUnchecked, GenValid)
import           Data.Validity (Validity(..), check)
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.Validity (genValidSpec, idempotentOnValid, eqSpecOnValid, ordSpecOnValid)

-- Instances for testing
instance Validity StreamPosition where
  validate (StreamPosition i) =
    check (i >= -1) "Position is either infimum or non-negative"

instance GenUnchecked StreamPosition
instance GenValid StreamPosition

-- Expect an 'Either a b' to contain a 'b' and fail otherwise.
expectRight :: Either a b -> b
expectRight (Right b) = b
expectRight (Left _) = error "Expected (Right _)"

-- Test suite for event store which stores 'ByteString' events.
streamPositionSpec :: Spec
streamPositionSpec = do
  genValidSpec @StreamPosition
  eqSpecOnValid @StreamPosition
  ordSpecOnValid @StreamPosition
  prop "bytesToStreamPosition and streamPositionToPosition should be inverses" $
    idempotentOnValid (expectRight . bytesToStreamPosition . streamPositionToBytes)
