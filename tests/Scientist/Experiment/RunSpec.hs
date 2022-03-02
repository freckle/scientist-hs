{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scientist.Experiment.RunSpec
  ( spec
  ) where

import Prelude

import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import Data.Time (NominalDiffTime)
import Scientist.Candidate
import Scientist.Control
import Scientist.Duration
import Scientist.Experiment
import Scientist.Experiment.Run
import Scientist.Result
import Test.Hspec
import UnliftIO.Concurrent
import UnliftIO.Exception (StringException(..), fromException, throwString)

data ExampleResult = A | B | C
  deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "experimentRun" $ do
    it "is ResultSkipped based on runIf" $ do
      result <-
        experimentRunInternal
        $ setExperimentRunIf False
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      case result of
        ResultSkipped (Control a) -> a `shouldBe` A
        _ -> expectationFailure "Expected result to be Skipped"

    it "is ResultSkipped based on enabled" $ do
      result <-
        experimentRunInternal
        $ setExperimentEnabled (pure False)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      case result of
        ResultSkipped (Control a) -> a `shouldBe` A
        _ -> expectationFailure "Expected result to be Skipped"

    it "is ResultSkipped when no Candidates present" $ do
      result <- experimentRunInternal $ newExperiment "test" (pure A)

      case result of
        ResultSkipped (Control a) -> a `shouldBe` A
        _ -> expectationFailure "Expected result to be Skipped"

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

    it "is ResultIgnored if any candidates are ignored" $ do
      let
        ignoreB _ = \case
          Right (Candidate B) -> True
          _ -> False

      result <-
        experimentRunInternal
        $ setExperimentIgnore ignoreB
        $ setExperimentTry (pure C)
        $ setExperimentTry (pure B)
        $ newExperiment "test" (pure A)

      case result of
        ResultIgnored{} -> pure ()
        _ -> expectationFailure "Expected result to be Ignored"

    it "is ResultMismatched if any candidates mismatched" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (pure B)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      case result of
        ResultMismatched{} -> pure ()
        _ -> expectationFailure "Expected result to be Mismatched"

    it "does not rescue exceptions in the Control branch" $ do
      let
        run = experimentRunInternal $ setExperimentTry (pure A) $ newExperiment
          "test"
          (throwString "boom")

      run `shouldThrow` isStringException "boom"

    it "rescues exceptions in the Candidate branch" $ do
      result <-
        experimentRunInternal
        $ setExperimentTry (throwString "boom")
        $ newExperiment "test" (pure A)

      case result of
        ResultMismatched rd -> do
          let
            mStringException =
              either fromException (const Nothing)
                $ resultCandidateValue
                $ NE.head
                $ resultDetailsCandidates rd

          mStringException
            `shouldSatisfy` maybe False (isStringException "boom")

        _ -> expectationFailure "Expected result to be Mismatched"

isDurationNear :: NominalDiffTime -> Duration -> Bool
isDurationNear x (Duration nd)
  | nd < x - tolerance = False
  | nd > x + tolerance = False
  | otherwise = True
  where tolerance = 0.050

isStringException :: String -> StringException -> Bool
isStringException a = \case
  StringException b _cs -> b == a
