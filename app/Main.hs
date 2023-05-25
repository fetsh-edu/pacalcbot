module Main (main) where

import Lib ( run, runDefault )
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
       [a] -> run a
       _ -> runDefault