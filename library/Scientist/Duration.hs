{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}



module Scientist.Duration
  ( Duration
  , measureDuration
  , toNanoSecs
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Data.Fixed (Fixed(..), Nano, showFixed)
import qualified System.Clock as Clock

-- | Time elapsed in seconds up to nanosecond precision
--
-- >> 1.005
-- 1.005s
--
-- >> toNanoSecs 1.005
-- 1005000000
newtype Duration = Duration
  { _unDuration :: Nano
  }
  deriving stock (Eq)
  deriving newtype (Enum, Ord, Num, Real, Fractional, RealFrac)

instance Show Duration where
  show (Duration x) = showFixed True x <> "s"

measureDuration :: MonadIO m => m a -> m (a, Duration)
measureDuration f = do
  begin <- liftIO getTime
  (,) <$> f <*> liftIO
    (Duration . MkFixed . Clock.toNanoSecs . subtract begin <$> getTime)
  where getTime = Clock.getTime Clock.Monotonic

toNanoSecs :: Duration -> Integer
toNanoSecs (Duration (MkFixed x)) = x
