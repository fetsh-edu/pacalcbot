module Calculator.Types
    ( Question (..)
    , Answer (..)
    , Unit (..)
    , Time (..)
    , Pace (..)
    , Distance (..)
    ) where

import Calculator.Unit (Unit(..))
import Calculator.Time (Time(..))
import Calculator.Distance (Distance(..))
import Calculator.Pace (Pace(..))
    
data Question
  = QPace Pace
  | QDistancePace Distance Pace
  | QDistanceTime Distance Time (Maybe Unit)
  | QPaceTime Pace Time (Maybe Unit)
  deriving (Show)

data Answer = Answer Distance Time Pace
  deriving (Show, Eq)
