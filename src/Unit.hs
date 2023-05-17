module Unit
    (Unit(..)
    , parser
    ) where

import Data.Char (toUpper)
import Text.Parsec (letter, many1, ParsecT, Stream)


data Unit = Meter | Kilometer | Mile
    deriving (Eq)

instance Show Unit where
    show Meter = "м"
    show Kilometer = "км"
    show Mile = "mi"
    
parser :: Stream s m Char => ParsecT s st m Unit
parser = do
  s <- many1 letter
  case fmap toUpper s of
    "КМ" -> return Kilometer
    "KM" -> return Kilometer
    "КИЛОМЕТР" -> return Kilometer
    "КИЛОМЕТРЫ" -> return Kilometer
    "МИЛЯ" -> return Mile
    "МИЛИ" -> return Mile
    "MILE" -> return Mile
    "MI" -> return Mile
    "М" -> return Mile
    "M" -> return Mile
    _ -> fail $ "km or mile or m expected: " ++ s

