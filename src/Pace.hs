{-# LANGUAGE RecordWildCards #-}

module Pace
    ( Pace (..)
    ) where

import Time (Time, minutes, seconds)
import Distance (Distance, unit, amount)
import Text.Printf (printf)

data Pace = Pace
  { time :: Time
  , distance :: Distance
  } deriving (Eq)

instance Show Pace where
  show Pace {..} = show (minutes time) <> ":" <> printf "%02d" (seconds time) <> " мин/" <> show (amount distance) <> show (unit distance)

