module Parser
    (parseQuestion) where

import Text.Parsec
import Types (Question(..))
import Unit (Unit(..))
import Pace (Pace(..))
import Time (Time(..))
import qualified Distance as D (Distance(..), marathon, halfMarathon)
import Data.Char (toUpper, isDigit, digitToInt)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace, traceShow)
import Data.Foldable (foldl')
import Data.List (isPrefixOf)

pPace :: Stream s m Char => ParsecT s u m Question
pPace = QPace <$> pace

unit :: Stream s m Char => ParsecT s st m Unit
unit = do
  s <- many1 letter
  case fmap toUpper s of
    "КМ" -> return Kilometer
    "KM" -> return Kilometer
    "КИЛОМЕТР" -> return Kilometer
    "КИЛОМЕТРЫ" -> return Kilometer
    "МИЛЯ" -> return Mile
    "МИЛИ" -> return Mile
    "MILE" -> return Mile
    "М" -> return Mile
    "M" -> return Mile
    _ -> fail $ "km or mile or m expected: " ++ s

pace :: Stream s m Char => ParsecT s st m Pace
pace = do
  _ <- optionMaybe $ choice [string "ПО", string "ТЕМП", string "С ТЕМПОМ", string "НА ТЕМПЕ"] 
  spaces
  m <- number 2 59
  _ <- oneOf [':', '.']
  s <- number 2 59
  spaces
  da <- option 1 (number 3 999)
  spaces
  u <- option Kilometer unit
  return $ Pace (Time 0 m s ) (D.Distance (fromIntegral da) u)

namedDistance :: Stream s m Char => ParsecT s st m D.Distance
namedDistance = try manyWordsHalfMarathon <|> try oneWordDistance

oneWordDistance :: Stream s m Char => ParsecT s st m D.Distance
oneWordDistance = do
    s <- many1 letter
    case fmap toUpper s of
        "МАРАФОН" -> return D.marathon
        "MARATHON" -> return D.marathon
        "HALFMARATHON" -> return D.halfMarathon
        "HALF" -> return D.halfMarathon
        "ПОЛОВИНКА" -> return D.halfMarathon
        "ПОЛУМАРАФОН" -> return D.halfMarathon
        "ПЯТЬ" -> return (D.Distance 5 Kilometer)
        "ПЯТИ" -> return (D.Distance 5 Kilometer)
        "ПЯТЕРКА" -> return (D.Distance 5 Kilometer)
        "ПЯТЁРКА" -> return (D.Distance 5 Kilometer)
        "ПАРКРАН" -> return (D.Distance 5 Kilometer)
        us ->
            if "ДЕСЯТ" `isPrefixOf` us
            then return (D.Distance 10 Kilometer)
            else if any (`isPrefixOf` us) ["ПЯТЕРК", "ПЯТЁРК", "ПАРКРАН"]
            then return (D.Distance 5 Kilometer)
            else if any (`isPrefixOf` us) ["МАРАФОН", "MARATHON"]
            then return D.marathon
            else if any (`isPrefixOf` us) ["HALFMARATHON", "HALF", "ПОЛОВИНК", "ПОЛУМАРАФОН"]
            then return D.halfMarathon
            else fail $ "one word distance fail: " ++ s


manyWordsHalfMarathon :: Stream s m Char => ParsecT s st m D.Distance
manyWordsHalfMarathon = D.halfMarathon <$ (string "HALF MARATHON" <|> string "ПОЛУ МАРАФОН" <|> string "HALF-MARATHON" <|> string "ПОЛУ-МАРАФОН")

time24 :: Stream s m Char => ParsecT s st m Time
time24 = do
  _ <- optionMaybe $ char ':'
  h <- number 2 23
  _ <- char ':'
  m <- number 2 59
  x <- optionMaybe $ char ':'
  case x of
    Nothing -> return $ Time h m 0
    Just _ -> do
      s <- number 2 59
      notFollowedBy letter
      return $ Time h m s

pDistancePace :: Stream s m Char => ParsecT s u m Question
pDistancePace = do
     d <- namedDistance
     skipMany (space <|> letter)
     QDistancePace d <$> pace

pTimeDistance :: Stream s m Char => ParsecT s u m Question
pTimeDistance = do
      d <- namedDistance
      skipMany (space <|> letter)
      t <- time24
      return $ QDistanceTime d t Nothing


pPaceTime = undefined

pQuestion :: Stream s m Char => ParsecT s u m Question
pQuestion
    = try pPace
    <|> try pDistancePace
    <|> try pTimeDistance
--    <|> try pPaceTime

-- | Parse date and time
parseQuestion
    :: String    -- ^ String to parse
    -> Either ParseError Question
parseQuestion = runParser pQuestion () "" . fmap toUpper


tryReadInt :: (Stream s m Char, Num a) => String -> ParsecT s st m a
tryReadInt str =
  if all isDigit str
    then return $ fromIntegral $ foldl (\a b -> 10 * a + digitToInt b) 0 str
    else fail $ "Cannot read: " ++ str

-- | Apply parser N times
times :: (Stream s m Char)
     => Int
     -> ParsecT s st m t
     -> ParsecT s st m [t]
times 0 _ = return []
times n p = do
  ts <- times (n-1) p
  t <- optionMaybe p
  case t of
    Just t' -> return (ts ++ [t'])
    Nothing -> return ts

times1 :: (Stream s m Char)
        => Int
        -> ParsecT s st m t
        -> ParsecT s st m [t]
times1 n p = do
    t <- times n p
    case t of
        [] -> fail "zero times"
        _ -> return t

--
-- | Parse natural number of N digits
-- which is not greater than M
number :: Stream s m Char
       => Int   -- ^ Number of digits
       -> Int   -- ^ Maximum value
       -> ParsecT s st m Int
number n m = do
  t <- tryReadInt =<< (n `times1` digit)
  if t > m
    then fail "number too large"
    else return t


--positiveNatural :: Stream s m Char => ParsecT s u m Int
--positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit