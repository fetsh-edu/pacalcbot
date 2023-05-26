{-# LANGUAGE FlexibleContexts #-}

module Parser.Finding
    ( Finding(..)
    , parseFindings ) where

import Text.Parsec
import Calculator.Pace (Pace)
import Calculator.Time (Time)
import qualified Parser.Time as Time (prefixedParser, parser)
import qualified Calculator.Distance as D (Distance)
import qualified Parser.Distance as D (parser)
import qualified Parser.Pace as Pace (parser, prefixedParser)
import Data.Char (toUpper)

data Finding_ = FPace Pace | FTime Time | FDistance D.Distance | FChar Char | EOF
    deriving (Show, Eq)

data Finding = RPace Pace | RTime Time | RDistance D.Distance
   deriving (Show, Eq)

findingsParser :: Stream s m Char => ParsecT s u m [Finding]
findingsParser = do
    r <- try (FPace <$> Pace.prefixedParser)
            <|> try (FTime <$> Time.prefixedParser)
            <|> try (FPace <$> Pace.parser)
            <|> try (FTime <$> Time.parser)
            <|> try (FDistance <$> D.parser)
            <|> (FChar <$> anyChar)
            <|> (EOF <$ eof)
    case r of
        FPace d -> do
            rest <- findingsParser
            pure $ RPace d : rest
        FDistance d -> do
            rest <- findingsParser
            pure $ RDistance d : rest
        FTime t -> do
            rest <- findingsParser
            pure $ RTime t : rest
        FChar _ ->
            findingsParser
        EOF ->
            pure []

parseFindings :: String -> Either ParseError [Finding]
parseFindings = runParser findingsParser () "" . fmap toUpper