module Main
  ( main
  ) where

import           Test.Hspec (hspec)
import           Data.CQRS.Types.StreamPositionTest

-- Run all the test suites.
main :: IO ()
main = do
  -- Run the full test set.
  hspec $ streamPositionSpec
