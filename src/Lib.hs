module Lib
    ( run
    , runDefault
    ) where

import Calculator.Types
import Data.List (nub)
import Calculator.Calculator (answer)
import Parser.Finding (Finding(..), parseFindings)
import Control.Monad ((>=>))
import Env (Config, getConfigByPath, getConfig)

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
            someFunc "если бежать half половинушку 3 30 mi из 2 16 по три пятнадцать mi 10к ну или марафонец из трех с чем-то"
--            let settings = BotSettings
--                    { airTableToken = airTable config
--                    , accountantId = Config.accountantId config
--                    , hrId = Config.hrId config
--                    , devId = Config.devId config
--                    , anonId = Config.anonId config
--                    , recordsParams = RecordsParams (recordsDatabase config) (recordsTable config) (recordsView config)
--                    , usersParams = UsersParams (usersDatabase config) (usersTable config)
--                    , taskParams = TaskParams (recordsDatabase config) (tasksTable config)
--                    , rolesParams = RolesParams (rolesDatabase config) (rolesTable config)
--                    }
--            env <- (defaultTelegramClientEnv . Token . telegram) config
--            bot <- initBot settings
--            let conversationBot_ = (sharedConversationBot updateChatId . traceTelegramUpdatesUglyJSON . traceBotActionsShow) bot
--            startBot_ conversationBot_ env

someFunc :: String -> IO ()
someFunc line  = print $ (findingsToQuestions >=> answer) <$> parseFindings line

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