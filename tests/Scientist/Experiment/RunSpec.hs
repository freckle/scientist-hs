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
        $ setExperimentTry (pure A)
        $ setExperimentTry (pure A)
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

          succeeded `shouldMatchList` [A, A]
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
        ResultIgnored rd -> do
          resultDetailsExperimentName rd `shouldBe` "test"

          let control = resultDetailsControl rd
          resultControlValue control `shouldBe` A

          let
            (failed, succeeded) =
              partitionEithers
                $ map resultCandidateValue
                $ NE.toList
                $ resultDetailsCandidates rd

          succeeded `shouldMatchList` [B, C]
          length failed `shouldBe` 0

        _ -> expectationFailure "Expected result to be Ignored"

    it "is ResultMismatched if any candidates mismatched" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (pure B)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      case result of
        ResultMismatched rd -> do
          resultDetailsExperimentName rd `shouldBe` "test"

          let control = resultDetailsControl rd
          resultControlValue control `shouldBe` A

          let
            (failed, succeeded) =
              partitionEithers
                $ map resultCandidateValue
                $ NE.toList
                $ resultDetailsCandidates rd

          succeeded `shouldMatchList` [A, B]
          length failed `shouldBe` 0

        _ -> expectationFailure "Expected result to be Mismatched"

    it "rescues exceptions in the Candidate branch" $ do
      result <-
        experimentRunInternal
        $ setExperimentTry (throwString "boom")
        $ newExperiment "test" (pure A)

      case result of
        ResultMismatched rd -> do
          resultDetailsExperimentName rd `shouldBe` "test"

          let
            control = resultDetailsControl rd
            mCandidateException =
              either fromException (const Nothing)
                $ resultCandidateValue
                $ NE.head
                $ resultDetailsCandidates rd

          resultControlValue control `shouldBe` A
          mCandidateException `shouldSatisfyMaybe` isStringException "boom"

        _ -> expectationFailure "Expected result to be Mismatched"

    it "does not rescue exceptions in the Control branch" $ do
      experimentRunInternal (newExperiment "test" (throwString "boom"))
        `shouldThrow` isStringException "boom"

    it "does not rescue exceptions in publishing" $ do
      experimentRunInternal
          (setExperimentPublish (\_ -> throwString "boom")
          $ setExperimentTry (pure B)
          $ newExperiment "test" (pure A)
          )
        `shouldThrow` isStringException "boom"

    it "can be configured to rescue exceptions in publishing" $ do
      result <-
        experimentRunInternal
        $ setExperimentOnException (\_ -> pure ())
        $ setExperimentPublish (\_ -> throwString "boom")
        $ setExperimentTry (pure B)
        $ newExperiment "test" (pure A)

      case result of
        ResultMismatched{} -> pure ()
        _ -> expectationFailure "Expected result to be Mismatched"

shouldSatisfyMaybe
  :: (HasCallStack, Show a) => Maybe a -> (a -> Bool) -> Expectation
x `shouldSatisfyMaybe` f = x `shouldSatisfy` maybe False f

infix 1 `shouldSatisfyMaybe`

isDurationNear :: NominalDiffTime -> Duration -> Bool
isDurationNear x (Duration nd)
  | nd < x - tolerance = False
  | nd > x + tolerance = False
  | otherwise = True
  where tolerance = 0.050

isStringException :: String -> StringException -> Bool
isStringException a = \case
  StringException b _cs -> b == a
