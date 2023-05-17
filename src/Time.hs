{-# LANGUAGE RecordWildCards #-}

module Time
    ( Time (..)
    , toSeconds
    , fromSeconds
    , multiply
    , divide
    , simpleParser
    , paceParser
    , prefixedParser
    , separator
    ) where

import Parser.Helpers (number, spaced)
import Text.Printf (printf)
import Text.Parsec (Stream, ParsecT, string, choice, notFollowedBy, optionMaybe, oneOf)


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
    simpleParser

paceParser :: Stream s m Char => ParsecT s st m Time
paceParser = do
    m <- number 2 59
    _ <- separator
    s <- number 2 59
    notFollowedBy separator
    return (Time 0 m s )

simpleParser :: Stream s m Char => ParsecT s st m Time
simpleParser = do
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
separator = oneOf [':', '.']