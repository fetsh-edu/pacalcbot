{-# LANGUAGE RecordWildCards #-}

module Calculator.Distance
    ( Distance (..)
    , marathon
    , halfMarathon
    , toMillimeters
    , tenK
    , fiveK
    , withUnit
    , map
    ) where

import Calculator.Unit (Unit(..))
import Prelude hiding (map)

data Distance = Distance
    { amount :: Double
    , unit :: Unit
    } deriving (Eq)

instance Show Distance where
    show Distance {..}
        | amount `almostEq`  42.195 = "Марафон"
        | amount `almostEq` 21.0975 = "Полумарафон"
        | otherwise = show amount <> " " <> show unit

almostEq :: (Ord a, Fractional a) => a -> a -> Bool
almostEq a b = abs (a - b) <= 0.0002

marathon :: Distance
marathon = Distance 42.195 Kilometer

halfMarathon :: Distance
halfMarathon = Distance 21.0975 Kilometer

tenK :: Distance
tenK = Distance 10 Kilometer

fiveK :: Distance
fiveK = Distance 5 Kilometer

toMillimeters :: Distance -> Double
toMillimeters Distance {..} =
    case unit of
        Meter -> amount * 1000
        Kilometer -> 1000000 * amount
        Mile -> 1609344 * amount


withUnit :: Distance -> Unit -> Distance
withUnit distance_ unit_ =
    case (unit distance_, unit_) of
        (Kilometer, Kilometer) -> distance_
        (Meter, Meter) -> distance_
        (Mile, Mile) -> distance_
        (Kilometer, Meter) -> Distance (amount distance_ * 1000) unit_
        (Meter, Kilometer) -> Distance (amount distance_ / 1000) unit_
        (Mile, Kilometer) -> Distance (amount distance_ / 0.6213711922) unit_
        (Kilometer, Mile) -> Distance (amount distance_ * 0.6213711922) unit_
        (Mile, Meter) -> withUnit (withUnit distance_ Kilometer) Meter
        (Meter, Mile) -> withUnit (withUnit distance_ Kilometer) Mile

map :: (Double -> Double) -> Distance -> Distance
map f d = d { amount = f (amount d) }