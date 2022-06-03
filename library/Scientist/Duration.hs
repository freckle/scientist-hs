{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scientist.Duration
  ( Duration
  , measureDuration
  , durationToSeconds
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Data.Fixed (Fixed(..), Nano, showFixed)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Word (Word64)

-- | Time elapsed in seconds up to nanosecond precision
--
-- >> 1.005
-- 1.005s
newtype Duration = Duration
  { _unDuration :: Nano
  }
  deriving stock (Eq)
  deriving newtype (Enum, Ord, Num, Real, Fractional, RealFrac)

instance Show Duration where
  show (Duration x) = showFixed True x <> "s"

measureDuration :: MonadIO m => m a -> m (a, Duration)
measureDuration f = do
  begin <- liftIO getMonotonicTimeNSec
  (,) <$> f <*> liftIO (fromNanoSecs . subtract begin <$> getMonotonicTimeNSec)

-- | Convert from duration to seconds
--
-- >> toSecs 0.000001
-- 0.000001
durationToSeconds :: Duration -> Double
durationToSeconds = realToFrac

-- | Convert to duration from nanoseconds
--
-- >> fromNanoSecs 1000
-- 0.000001s
fromNanoSecs :: Word64 -> Duration
fromNanoSecs = Duration . MkFixed . fromIntegral
