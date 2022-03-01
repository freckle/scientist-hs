{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Scientist.Result
  ( Result(..)
  , resultValue
  , ResultDetails(..)
  , ResultControl(..)
  , runControl
  , ResultCandidate(..)
  , runCandidate
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Scientist.Candidate
import Scientist.Control
import Scientist.Duration
import UnliftIO.Exception (SomeException, tryAny)

data Result c a b
    = ResultSkipped (Control a)
    | ResultMatched (ResultDetails c a b)
    | ResultIgnored (ResultDetails c a b)
    | ResultMismatched (ResultDetails c a b)

resultValue :: Result c a b -> a
resultValue = \case
  ResultSkipped (Control a) -> a
  ResultMatched rd -> resultDetailsControlValue rd
  ResultIgnored rd -> resultDetailsControlValue rd
  ResultMismatched rd -> resultDetailsControlValue rd

data ResultDetails c a b = ResultDetails
  { resultDetailsExperimentName :: Text
  , resultDetailsExperimentContext :: Maybe c
  , resultDetailsControl :: ResultControl a
  , resultDetailsCandidates :: NonEmpty (ResultCandidate b)
  }

resultDetailsControlValue :: ResultDetails c a b -> a
resultDetailsControlValue = resultControlValue . resultDetailsControl

-- TODO: name, cleaned_value
data ResultControl a = ResultControl
  { resultControlValue :: a
  , resultControlDuration :: Duration
  }

runControl :: MonadIO m => m (Control a) -> m (ResultControl a)
runControl control = do
  (Control a, d) <- measureDuration control
  pure ResultControl { resultControlValue = a, resultControlDuration = d }

-- TODO: name, cleaned_value
data ResultCandidate a = ResultCandidate
  { resultCandidateValue :: Either SomeException a
  , resultCandidateDuration :: Duration
  }

runCandidate :: MonadUnliftIO m => m (Candidate b) -> m (ResultCandidate b)
runCandidate candidate = do
  (b, d) <- measureDuration $ tryAny candidate
  pure $ ResultCandidate
    { resultCandidateValue = unCandidate <$> b
    , resultCandidateDuration = d
    }

--  x.raised?
--  x.exception.class/message/backtrace
--  x.value
--  x.cleaned_value
-- data ResultItemDetails a = ResultItemDetails
--     { resultItemName :: Maybe Text
--     , resultItemDuration :: Duration
--     , resultItemValue :: Either SomeException a
--     }
