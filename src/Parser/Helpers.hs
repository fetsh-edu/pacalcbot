module Parser.Helpers
    ( number
    , spaced
    ) where

import Text.Parsec (Stream, ParsecT, digit, optionMaybe, spaces)
import Data.Char (digitToInt, isDigit)


spaced :: Stream s m Char => ParsecT s st m a -> ParsecT s st m a
spaced someParser = do
    spaces
    m <- someParser
    spaces
    return m


tryReadInt :: (Num a) => String -> ParsecT s st m a
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


