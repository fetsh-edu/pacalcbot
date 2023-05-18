module Parser.Distance
    (parser) where

  
import qualified Parser.Unit as Unit (parser)
import Calculator.Distance
import Calculator.Unit as Unit (Unit(..))
import Text.Parsec (Stream, ParsecT, try, (<|>), letter, many1, string, option)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Parser.Helpers (spaced, number)
  
  
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

