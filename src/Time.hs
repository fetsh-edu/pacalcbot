{-# LANGUAGE RecordWildCards #-}

module Time
    ( Time (..)
    , toSeconds
    , fromSeconds
    , multiply
    , divide
    , parser
    , paceTimeParser
    , prefixedParser
    , separator
    , asWords
    ) where

import Parser.Helpers (number, spaced)
import Text.Printf (printf)
import Text.Parsec (Stream, ParsecT, string, choice, notFollowedBy, optionMaybe, oneOf, spaces, (<|>), skipMany, char, try)
import Control.Monad (void)


data Time = Time
    { hours :: Int
    , minutes :: Int
    , seconds :: Int
    } deriving (Eq)
    

instance Show Time where
    show Time {..} = show hours <> ":" <> printf "%02d" minutes <> ":" <> printf "%02d" seconds


toSeconds :: Time -> Int
toSeconds Time {..} = seconds + minutes * 60 + hours * 60 * 60

fromSeconds :: Int -> Time
fromSeconds secs_ = Time { hours = hours_, minutes = minutes_, seconds = seconds_ }
    where
        (hours_, minutes__) = secs_ `divMod` (60 * 60)
        (minutes_, seconds_) = minutes__ `divMod` 60

multiply :: Double -> Time -> Time
multiply m t = fromSeconds $ round (m * fromIntegral (toSeconds t))

divide :: Double -> Time -> Time
divide m t = fromSeconds $ round (fromIntegral (toSeconds t) / m)

prefix :: Stream s m Char => ParsecT s st m String
prefix = choice [string "ИЗ", string "ЗА"]

prefixedParser :: Stream s m Char => ParsecT s st m Time
prefixedParser = do
    _ <- spaced prefix
    asNumbers

parser :: Stream s m Char => ParsecT s st m Time
parser = asNumbers <|> asWords

paceTimeParser :: Stream s m Char => ParsecT s st m Time
paceTimeParser = paceTimeAsNumber <|> paceTimeAsWords

paceTimeAsNumber :: Stream s m Char => ParsecT s st m Time
paceTimeAsNumber = do
    m <- number 2 59
    _ <- separator
    s <- number 2 59
    notFollowedBy $ oneOf [':', '.']
    return (Time 0 m s )

paceTimeAsWords :: Stream s m Char => ParsecT s st m Time
paceTimeAsWords = do
    m <- minutesWord
    spaced minutesSeparator
    Time 0 m <$> fiveToSixtyWords

asWords :: Stream s m Char => ParsecT s st m Time
asWords = do
    h <- hoursWord
    spaced hoursSeparator
    m <- fiveToSixtyWords
    return $ Time h m 0

hoursWord :: Stream s m Char => ParsecT s st m Int
hoursWord
    =   try (1 <$ string "ЧАС")
    <|> try (1 <$ string "ЧАСА")
    <|> oneToNineWords

minutesWord :: Stream s m Char => ParsecT s st m Int
minutesWord
    =   try (1 <$ string "МИНУТА")
    <|> try (1 <$ string "МИНУТЕ")
    <|> oneToNineWords

oneToNineWords :: Stream s m Char => ParsecT s st m Int
oneToNineWords
    =   try (1 <$ string "ОДНА")
    <|> try (1 <$ string "ОДИН")
    <|> try (1 <$ string "ОДНОГО")
    <|> try (1 <$ string "ОДНОЙ")
    <|> try (2 <$ string "ДВА")
    <|> try (2 <$ string "ДВЕ")
    <|> try (2 <$ string "ДВУХ")
    <|> try (3 <$ string "ТРИ")
    <|> try (3 <$ string "ТРЕХ")
    <|> try (3 <$ string "ТРЁХ")
    <|> try (4 <$ string "ЧЕТЫРЕ")
    <|> try (4 <$ string "ЧЕТЫРЁХ")
    <|> try (5 <$ string "ПЯТЬ")
    <|> try (5 <$ string "ПЯТИ")
    <|> try (6 <$ string "ШЕСТЬ")
    <|> try (6 <$ string "ШЕСТИ")
    <|> try (7 <$ string "СЕМЬ")
    <|> try (7 <$ string "СЕМИ")
    <|> try (8 <$ string "ВОСЕМЬ")
    <|> try (8 <$ string "ВОСЬМИ")
    <|> try (9 <$ string "ДЕВЯТЬ")
    <|> try (9 <$ string "ДЕВЯТИ")

fiveToSixtyWords :: Stream s m Char => ParsecT s st m Int
fiveToSixtyWords
    =   try (5 <$ string "НОЛЬ ПЯТЬ")
    <|> try (5 <$ string "НОЛЬ ПЯТИ")
    <|> try (55 <$ string "ПЯТИДЕСЯТИ ПЯТИ")
    <|> try (55 <$ string "ПЯТЬДЕСЯТ ПЯТЬ")
    <|> try (50 <$ string "ПЯТИДЕСЯТИ")
    <|> try (50 <$ string "ПЯТЬДЕСЯТ")
    <|> try (5 <$ string "ПЯТЬ")
    <|> try (5 <$ string "ПЯТИ")
    <|> try (10 <$ string "ДЕСЯТЬ")
    <|> try (10 <$ string "ДЕСЯТИ")
    <|> try (15 <$ string "ПЯТНАДЦАТЬ")
    <|> try (15 <$ string "ПЯТНАДЦАТИ")
    <|> try (25 <$ string "ДВАДЦАТЬ ПЯТЬ")
    <|> try (25 <$ string "ДВАДЦАТИ ПЯТИ")
    <|> try (20 <$ string "ДВАДЦАТЬ")
    <|> try (20 <$ string "ДВАДЦАТИ")
    <|> try (35 <$ string "ТРИДЦАТЬ ПЯТЬ")
    <|> try (35 <$ string "ТРИДЦАТИ ПЯТИ")
    <|> try (30 <$ string "ТРИДЦАТЬ")
    <|> try (30 <$ string "ТРИДЦАТИ")
    <|> try (45 <$ string "СОРОКА ПЯТИ")
    <|> try (45 <$ string "СОРОК ПЯТЬ")
    <|> try (40 <$ string "СОРОКА")
    <|> try (40 <$ string "СОРОК")
    <|> try (30 <$ string "ПОЛОВИНОЙ")
    <|> try (30 <$ string "ЧЕМ-ТО")
    <|> try (15 <$ string "ЧЕТВЕРТЬЮ")


asNumbers :: Stream s m Char => ParsecT s st m Time
asNumbers = do
  h <- number 2 23
  _ <- separator
  m <- number 2 59
  x <- optionMaybe separator
  case x of
    Nothing -> return $ Time h m 0
    Just _ -> do
      s <- number 2 59
      return $ Time h m s

separator :: Stream s m Char => ParsecT s st m Char
separator = oneOf [':', '.', ' ']

hoursSeparator :: Stream s m Char => ParsecT s st m ()
hoursSeparator = skipMany $ try (string "ЧАСОВ") <|> try (string "ЧАСА") <|> try (string "ЧАС") <|> try (string "С ")

minutesSeparator :: Stream s m Char => ParsecT s st m ()
minutesSeparator = skipMany $ try (string "МИНУТЫ") <|> try (string "МИНУТА") <|> try (string "МИНУТ") <|> try (string "С ")