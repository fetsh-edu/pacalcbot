{-# LANGUAGE RecordWildCards #-}

module Distance
    ( Distance (..)
    , marathon
    , halfMarathon
    , toMillimeters
    , withUnit
    , map
    , parser
    ) where

import Unit (Unit(..))
import qualified Unit (parser)
import Prelude hiding (map)
import Text.Parsec (Stream, ParsecT, try, (<|>), letter, many1, string, option)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Parser.Helpers (spaced, number)

data Distance = Distance
    { amount :: Double
    , unit :: Unit
    } deriving (Eq)

instance Show Distance where
    show Distance {..}
        | amount `almostEq`  42.195 = "Марафон (42.195 км)"
        | amount `almostEq` 21.0975 = "Полумарафон (21.0975 км)"
        | otherwise = show amount <> ": " <> show unit

almostEq :: (Ord a, Fractional a) => a -> a -> Bool
almostEq a b = abs (a - b) <= 0.0002

marathon :: Distance
marathon = Distance 42.195 Kilometer

halfMarathon :: Distance
halfMarathon = Distance 21.0975 Kilometer

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

parser :: Stream s m Char => ParsecT s st m Distance
parser = namedDistance <|> numberWithUnit

numberWithUnit :: Stream s m Char => ParsecT s st m Distance
numberWithUnit = do
    d <- spaced $ fromIntegral <$> number 4 9999 
    u <- option Unit.Kilometer (try (spaced Unit.parser))
    return $ Distance d u

namedDistance :: Stream s m Char => ParsecT s st m Distance
namedDistance = try manyWordsHalfMarathon <|> try oneWordDistance

oneWordDistance :: Stream s m Char => ParsecT s st m Distance
oneWordDistance = do
    s <- many1 letter
    case fmap toUpper s of
        "МАРАФОН" -> return marathon
        "MARATHON" -> return marathon
        "HALFMARATHON" -> return halfMarathon
        "HALF" -> return halfMarathon
        "ПОЛОВИНКА" -> return halfMarathon
        "ПОЛУМАРАФОН" -> return halfMarathon
        "ПЯТЬ" -> return (Distance 5 Kilometer)
        "ПЯТИ" -> return (Distance 5 Kilometer)
        "ПЯТЕРКА" -> return (Distance 5 Kilometer)
        "ПЯТЁРКА" -> return (Distance 5 Kilometer)
        "ПАРКРАН" -> return (Distance 5 Kilometer)
        us ->
            if "ДЕСЯТ" `isPrefixOf` us
            then return (Distance 10 Kilometer)
            else if any (`isPrefixOf` us) ["ПЯТЕР", "ПЯТЁР", "ПАРКРАН"]
            then return (Distance 5 Kilometer)
            else if any (`isPrefixOf` us) ["МАРАФОН", "MARATHON"]
            then return marathon
            else if any (`isPrefixOf` us) ["HALFMARATHON", "HALF", "ПОЛОВИН", "ПОЛУМАРАФОН"]
            then return halfMarathon
            else fail $ "one word distance fail: " ++ s


manyWordsHalfMarathon :: Stream s m Char => ParsecT s st m Distance
manyWordsHalfMarathon = halfMarathon <$ (string "HALF MARATHON" <|> string "ПОЛУ МАРАФОН" <|> string "HALF-MARATHON" <|> string "ПОЛУ-МАРАФОН")