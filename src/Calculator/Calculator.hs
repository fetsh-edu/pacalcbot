module Calculator.Calculator
    ( answer ) where


import Data.Maybe (fromMaybe)
import Calculator.Distance as D (toMillimeters, map, withUnit, marathon, halfMarathon, fiveK, tenK)
import Calculator.Types
import Calculator.Time (multiply, divide, toSeconds)

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
answer (QPace pace_) = flip fromDistanceAndPace pace_ <$> [marathon, halfMarathon, tenK, fiveK]
answer (QDistancePace distance_ pace_) = [fromDistanceAndPace distance_ pace_]
answer (QDistanceTime distance_ time_ unit_) = [fromDistanceAndTime distance_ time_ unit_]
answer (QPaceTime pace_ time_ unit_) = [fromPaceAndTime pace_ time_ unit_]

