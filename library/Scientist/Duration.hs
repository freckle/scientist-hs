{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scientist.Duration
  ( Duration(..)
  , measureDuration
  , toNanoSecs
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import qualified System.Clock as Clock

-- | Time elapsed in nano seconds
newtype Duration = Duration
  { unDuration :: Integer
  }
  deriving stock (Eq, Show)
  deriving newtype (Enum, Ord, Num, Real, Integral)

measureDuration :: MonadIO m => m a -> m (a, Duration)
measureDuration f = do
  begin <- liftIO getTime
  (,) <$> f <*> liftIO
    (Duration . Clock.toNanoSecs . subtract begin <$> getTime)
  where getTime = Clock.getTime Clock.Monotonic

toNanoSecs :: Duration -> Integer
toNanoSecs = unDuration
