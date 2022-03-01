{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Scientist.Experiment.RunSpec
  ( spec
  ) where

import Prelude

import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import Data.Time (NominalDiffTime)
import Scientist.Duration
import Scientist.Experiment
import Scientist.Experiment.Run
import Scientist.Result
import Test.Hspec
import UnliftIO.Concurrent

data ExampleResult = A | B | C
  deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "experimentRun" $ do
    -- it is ResultSkipped based on runIf

    -- it is ResultSkipped based on enabled

    -- it is ResultSkipped when no Candidates present

    it "is ResultMatched if all candidates match" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (A <$ threadDelay (150 * 1000))
        $ setExperimentTry (A <$ threadDelay (200 * 1000))
        $ newExperiment "test" (A <$ threadDelay (100 * 1000))

      case result of
        ResultMatched rd -> do
          resultDetailsExperimentName rd `shouldBe` "test"

          let control = resultDetailsControl rd
          resultControlValue control `shouldBe` A
          resultControlDuration control `shouldSatisfy` isDurationNear 0.100

          let
            (failed, succeeded) =
              partitionEithers
                $ map resultCandidateValue
                $ NE.toList
                $ resultDetailsCandidates rd

          succeeded `shouldBe` [A, A]
          length failed `shouldBe` 0

        _ -> expectationFailure "Expected result to be Matched"

    -- it is ResultIgnored if any candidates are ignored

    -- it is ResultMismatched if any candidates mismatched

    -- it does not rescue exceptions in the Control branch

    -- it rescues exceptions in the Candidate branch

    -- it errors if no Control present

isDurationNear :: NominalDiffTime -> Duration -> Bool
isDurationNear x (Duration nd)
  | nd < x - tolerance = False
  | nd > x + tolerance = False
  | otherwise = True
  where tolerance = 0.050
