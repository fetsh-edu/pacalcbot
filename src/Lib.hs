module Lib
    ( run
    , runDefault
    ) where

import Env (Config(..), getConfigByPath, getConfig)
import qualified Bot.Bot as Bot (runWebHook, runPolling, runWebHook)
import System.Environment (lookupEnv)

run :: String -> IO ()
run configPath_ = getConfigByPath configPath_ >>= runWithConfig

runDefault :: IO ()
runDefault = getConfig >>= runWithConfig

runWithConfig :: Show a => Either a Config -> IO ()
runWithConfig eitherConfig =
    case eitherConfig of
        Left exc -> fail $ "Could not parse file: " ++ show exc
        Right config -> do
            print config
            env <- lookupEnv "BOT_ENV" 
            case env of
                Just "production" ->  Bot.runWebHook (telegram config) 22455 "127.0.0.1"
                _ -> Bot.runPolling (telegram config)