module Parser.Pace
    ( parser
    , prefixedParser
    ) where

import Parser.Time (paceTimeParser)
import Calculator.Pace (Pace(..))
import Calculator.Distance (Distance(..))
import qualified Calculator.Unit as Unit (Unit(..))
import qualified Parser.Unit as Unit (forPaceParser)
import Text.Parsec (Stream, ParsecT, option, try, choice, string)
import Parser.Helpers (spaced)

prefixedParser :: Stream s m Char => ParsecT s st m Pace
prefixedParser = do
    _ <- spaced prefix
    parser

parser :: Stream s m Char => ParsecT s st m Pace
parser = do
    t <- spaced paceTimeParser
    u <- option Unit.Kilometer (try (spaced Unit.forPaceParser))
    return $ Pace t (Distance 1 u)

prefix :: Stream s m Char => ParsecT s st m String
prefix = choice [string "ПО", string "ТЕМП", string "С ТЕМПОМ", string "НА ТЕМПЕ"]

