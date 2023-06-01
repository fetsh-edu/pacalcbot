{-# LANGUAGE FlexibleContexts #-}

module Parser.Time
    ( parser
    , paceTimeParser
    , prefixedParser
    , asWords
    , asNumbers
    ) where

import Calculator.Time
import Parser.Helpers (number, spaced)
import Text.Parsec (Stream, ParsecT, string, choice, notFollowedBy, optionMaybe, oneOf, (<|>), skipMany, try)

prefix :: Stream s m Char => ParsecT s st m String
prefix = choice [string "ИЗ", string "ЗА"]

prefixedParser :: Stream s m Char => ParsecT s st m Time
prefixedParser = do
    _ <- spaced prefix
    try parser <|>  try minutesAsOneNumber

parser :: Stream s m Char => ParsecT s st m Time
parser = spaced (asNumbers <|> asWords)

paceTimeParser :: Stream s m Char => ParsecT s st m Time
paceTimeParser = paceTimeAsNumber <|> paceTimeAsWords

paceTimeAsNumber :: Stream s m Char => ParsecT s st m Time
paceTimeAsNumber = do
    m <- number 2 59
    _ <- separator
    s <- number 2 59
    notFollowedBy strictSeparator
    return (Time 0 m s )

paceTimeAsWords :: Stream s m Char => ParsecT s st m Time
paceTimeAsWords = do
    m <- minutesWord
    spaced minutesSeparator
    s <- optionMaybe fiveToSixtyWords
    case s of
        Nothing -> return $ Time 0 m 0
        Just s_ -> return $ Time 0 m s_

asWords :: Stream s m Char => ParsecT s st m Time
asWords = do
    h <- hoursWord
    spaced hoursSeparator
    m <- optionMaybe fiveToSixtyWords
    case m of
        Nothing -> return $ Time h 0 0
        Just m_ -> return $ Time h m_ 0

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
  x <- optionMaybe strictSeparator
  case x of
    Nothing -> return $ Time h m 0
    Just _ -> do
      s <- try (number 2 59)
      return $ Time h m s

minutesAsOneNumber :: Stream s m Char => ParsecT s st m Time
minutesAsOneNumber = do
    (\x -> Time 0 x 0) <$> spaced (number 2 99)
    


separator :: Stream s m Char => ParsecT s st m Char
separator = oneOf [':', '.', ' ']

strictSeparator :: Stream s m Char => ParsecT s st m Char
strictSeparator = oneOf [':', '.']

hoursSeparator :: Stream s m Char => ParsecT s st m ()
hoursSeparator = skipMany $ try (string "ЧАСОВ") <|> try (string "ЧАСА") <|> try (string "ЧАС") <|> try (string "С ")

minutesSeparator :: Stream s m Char => ParsecT s st m ()
minutesSeparator = skipMany $ try (string "МИНУТЫ") <|> try (string "МИНУТА") <|> try (string "МИНУТ") <|> try (string "С ")