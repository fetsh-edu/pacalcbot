{-# LANGUAGE RecordWildCards #-}

module Pace
    ( Pace (..)
    , simpleParser
    , prefixedParser
    ) where

import Time (Time, minutes, seconds, paceParser)
import Distance (Distance(..), unit, amount)
import qualified Unit (parser, Unit(..))
import Text.Printf (printf)
import Text.Parsec (Stream, ParsecT, option, try, choice, string)
import Parser.Helpers (spaced)

data Pace = Pace
  { time :: Time
  , distance :: Distance
  } deriving (Eq)

instance Show Pace where
  show Pace {..} = show (minutes time) <> ":" <> printf "%02d" (seconds time) <> " мин/" <> show (amount distance) <> show (unit distance)



prefixedParser :: Stream s m Char => ParsecT s st m Pace
prefixedParser = do
    _ <- spaced prefix
    simpleParser

simpleParser :: Stream s m Char => ParsecT s st m Pace
simpleParser = do
    t <- spaced paceParser
    u <- option Unit.Kilometer (try (spaced Unit.parser))
    return $ Pace t (Distance 1 u)

prefix :: Stream s m Char => ParsecT s st m String
prefix = choice [string "ПО", string "ТЕМП", string "С ТЕМПОМ", string "НА ТЕМПЕ"]



