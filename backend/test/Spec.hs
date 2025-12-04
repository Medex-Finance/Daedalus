module Main (main) where

import Test.Hspec

import qualified EvidenceSpec
import qualified OrchestratorSpec

main :: IO ()
main = hspec $ do
  OrchestratorSpec.spec
  EvidenceSpec.spec
