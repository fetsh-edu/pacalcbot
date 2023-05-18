{-# LANGUAGE RecordWildCards #-}

module Calculator.Pace
    ( Pace (..)
    ) where

import Calculator.Time (Time, minutes, seconds)
import Calculator.Distance (Distance(..), unit, amount)
import Text.Printf (printf)

data Pace = Pace
  { time :: Time
  , distance :: Distance
  } deriving (Eq)

instance Show Pace where
  show Pace {..} = show (minutes time) <> ":" <> printf "%02d" (seconds time) <> " мин/" <> show (amount distance) <> show (unit distance)
