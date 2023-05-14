{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    , answer
    , fromDistanceAndPace
    , fromDistanceAndTime
    , fromPaceAndTime
    , Time (..)
    , Distance (..)
    , Pace (..)
    , Unit (..)
    , Question (..)
    ) where

import Data.Maybe (fromMaybe)
import Distance as D (Distance (..), toMillimeters, map, withUnit, marathon, halfMarathon)
import Time as T
import Unit (Unit (..))
import Text.Printf (printf)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Pace = Pace
    { time :: Time
    , distance :: Distance
    } deriving (Eq)

instance Show Pace where
    show Pace {..} = show (minutes time) <> ":" <> printf "%02d" (seconds time) <> " мин/" <> show (unit distance)


data Question
    = QPace Pace
    | QDistancePace Distance Pace
    | QDistanceTime Distance Time (Maybe Unit)
    | QPaceTime Pace Time (Maybe Unit)
    deriving (Show)

data Answer = Answer Distance Time Pace
    deriving (Show)

fromDistanceAndPace :: Distance -> Pace -> Answer
fromDistanceAndPace distance_ pace_ = Answer distance_ time_ pace_
    where
        time_ = multiply distanceDiv $ time pace_
        distanceDiv = toMillimeters distance_ / toMillimeters (distance pace_)

fromDistanceAndTime :: Distance -> Time -> Maybe Unit -> Answer
fromDistanceAndTime distance_ time_ unit_ = Answer distance_ time__ pace_
    where
        paceDistance = Distance 1 (fromMaybe (unit distance_) unit_)
        distanceDiv = toMillimeters distance_ / toMillimeters paceDistance
        pace_ = Pace { time = divide distanceDiv time_ ,distance = paceDistance }
        Answer _ time__ _ = fromDistanceAndPace distance_ pace_

fromPaceAndTime :: Pace -> Time -> Maybe Unit -> Answer
fromPaceAndTime pace_ time_ unit_ = Answer distance_ time_ pace_
    where
        timeDiv = fromIntegral (toSeconds time_) / fromIntegral (toSeconds (time pace_))
        paceDistance_ = (timeDiv *) `D.map` distance pace_
        distance_ = maybe paceDistance_ (D.withUnit paceDistance_) unit_


answer :: Question -> [Answer]
answer (QPace pace_) = flip fromDistanceAndPace pace_ <$> [marathon, halfMarathon]
answer (QDistancePace distance_ pace_) = [fromDistanceAndPace distance_ pace_]
answer (QDistanceTime distance_ time_ unit_) = [fromDistanceAndTime distance_ time_ unit_]
answer (QPaceTime pace_ time_ unit_) = [fromPaceAndTime pace_ time_ unit_]