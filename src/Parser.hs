module Parser
    (parseFindings, findingsParser) where

import Text.Parsec
import Pace (Pace(..))
import Time (Time(..))
import qualified Time (prefixedParser, simpleParser)
import qualified Distance as D (Distance(..), parser)
import qualified Pace (simpleParser, prefixedParser)
import Data.Char (toUpper)
import Debug.Trace (trace)


data Finding = FPace Pace | FTime Time | FDistance D.Distance | FChar Char | EOF
    deriving (Show, Eq)

data RelevantFinding = RPace Pace | RTime Time | RDistance D.Distance
   deriving (Show, Eq)

findingsParser :: Stream s m Char => ParsecT s u m [RelevantFinding]
findingsParser = do
    r <- try (FPace <$> Pace.prefixedParser)
            <|> try (FTime <$> Time.prefixedParser)
            <|> try (FPace <$> Pace.simpleParser)
            <|> try (FTime <$> Time.simpleParser)
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

parseFindings :: String -> Either ParseError [RelevantFinding]
parseFindings = runParser findingsParser () "" . fmap toUpper