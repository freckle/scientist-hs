{-# LANGUAGE RankNTypes #-}

module Scientist.Experiment.Run
  ( experimentRun
  , experimentRunInternal
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.Bifunctor (second)
import Data.Bitraversable (bimapM)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Scientist.Experiment
import Scientist.Result
import Scientist.Result.Evaluate
import System.Random.Shuffle (shuffleM)
import UnliftIO.Exception (handleAny, throwString)

experimentRun :: MonadUnliftIO m => Experiment m c a b -> m a
experimentRun = fmap resultValue . experimentRunInternal

-- | 'experimentRun' but returning the full 'Result'
--
-- Used for testing.
--
experimentRunInternal
  :: MonadUnliftIO m => Experiment m c a b -> m (Result c a b)
experimentRunInternal ex = do
  disabled <- experimentDisabled ex

  case getExperimentTries ex of
    Just candidates | not disabled -> do
      (controlResult, candidateResults) <- runRandomized
        control
        candidates
        runControl
        runCandidate

      let result = evaluateResult ex controlResult candidateResults

      result <$ handleAny
        (getExperimentOnException ex)
        (getExperimentPublish ex result)

    _ -> ResultSkipped <$> control
  where control = getExperimentUse ex

experimentDisabled :: Applicative m => Experiment m c a b -> m Bool
experimentDisabled ex
  | not (getExperimentRunIf ex) = pure False
  | otherwise = not <$> getExperimentEnabled ex

runRandomized
  :: MonadIO m
  => a
  -> NonEmpty b
  -> (a -> m a') -- ^ How to run the @a@
  -> (b -> m b') -- ^ How to run each @b@
  -> m (a', NonEmpty b')
runRandomized a bs runA runB = do
  inputs <- liftIO $ shuffleM $ Left a : map Right (NE.toList bs)
  outputs <- traverse (bimapM runA runB) inputs

  let partitioned = partitionEithers outputs

  case second NE.nonEmpty partitioned of
    ([a'], Just bs') -> pure (a', bs')
    _ ->
      -- Justification for this being "impossible":
      --
      -- - We cannot produce an a or b out of thin air
      -- - We were given an a and NonEmpty b
      -- - We cannot forget to use a without an unused warning
      -- - We cannot forget to use bs without an unused warning
      -- - We're doing no filtering anywhere
      --
      -- Therefore, there's no way to not get 1 Left and 1+ Rights here.
      --
      throwString
        $ "runRandomized did not produce 1 Left and 1+ Rights, but "
        <> show (length $ fst partitioned)
        <> " Left(s), and "
        <> show (length $ snd partitioned)
        <> " Rights(s)"
