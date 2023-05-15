module Types
    ( Question (..)
    , Answer (..)
    ) where

import Unit (Unit)
import Time (Time)
import Distance (Distance)
import Pace (Pace)
    
data Question
  = QPace Pace
  | QDistancePace Distance Pace
  | QDistanceTime Distance Time (Maybe Unit)
  | QPaceTime Pace Time (Maybe Unit)
  deriving (Show)

data Answer = Answer Distance Time Pace
  deriving (Show)

