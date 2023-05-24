module Lib
    ( someFunc
    ) where

import Calculator.Types
import Data.List (nub)
import Calculator.Calculator (answer)
import Parser.Finding (Finding(..), parseFindings)
import Control.Monad ((>=>))

someFunc :: String -> IO ()
someFunc line  = print $ (findingsToQuestions >=> answer) <$> parseFindings line

-- из 90

findingsToQuestions :: [Finding] -> [Question]
findingsToQuestions findings
  | length findings == length paces = QPace <$> paces
  | not (null distances) = (QDistancePace <$> distances <*> paces) <> ((\ x y -> QDistanceTime x y Nothing) <$> distances <*> times)
  | otherwise = (\x y -> QPaceTime x y Nothing) <$> paces <*> times
  where
      qFindings = nub findings
      paces = [ x | RPace x <- qFindings ]
      distances = [ x | RDistance x <- qFindings ]
      times = [ x | RTime x <- qFindings ]