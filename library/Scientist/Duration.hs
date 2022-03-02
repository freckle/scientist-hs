{-# LANGUAGE DerivingStrategies #-}

module Scientist.Duration
  ( Duration(..)
  , measureDuration
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

newtype Duration = Duration
  { unDuration :: NominalDiffTime
  }
  deriving stock (Eq, Show)

measureDuration :: MonadIO m => m a -> m (a, Duration)
measureDuration f = do
  begin <- liftIO getCurrentTime
  (,) <$> f <*> liftIO (Duration . (`diffUTCTime` begin) <$> getCurrentTime)
