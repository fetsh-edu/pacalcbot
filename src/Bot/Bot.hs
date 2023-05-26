module Bot.Bot
  ( runWebHook
  , runPolling
  ) where

import Data.Maybe (isJust)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    setPort,
  )
import Network.Wai.Handler.WarpTLS
  ( OnInsecure (AllowInsecure),
    TLSSettings (onInsecure),
    defaultTlsSettings,
    tlsSettings,
  )
import Telegram.Bot.API
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
  ( updateMessageSticker,
    updateMessageText,
  )
import Calculator.Types
import Data.List (nub)
import Calculator.Calculator (answer)
import Parser.Finding (Finding(..), parseFindings)
import Control.Monad ((>=>))
import Text.Parsec (ParseError)


type Model = ()

data Action
  = InlineIncoming InlineQueryId Text
  | Incoming Text

pacalcBot :: BotApp Model Action
pacalcBot =
  BotApp
    { botInitialModel = (),
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs = []
    }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateInlineQuery update = do
    query <- updateInlineQuery update
    let queryId = inlineQueryId query
    let msg = inlineQueryQuery query
    Just $ InlineIncoming queryId msg
  | otherwise = case updateMessageText update of
    Just text -> Just (Incoming text)
    Nothing -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  InlineIncoming queryId msg ->
    model <# do
      let result =
            (defInlineQueryResultGeneric (InlineQueryResultId msg))
              { inlineQueryResultTitle = Just msg,
                inlineQueryResultInputMessageContent = Just (defaultInputTextMessageContent msg)
              }
          thumbnail = defInlineQueryResultGenericThumbnail result
          article = defInlineQueryResultArticle thumbnail
          answerInlineQueryRequest = defAnswerInlineQuery queryId [article]
      _ <- runTG answerInlineQueryRequest
      return ()
  Incoming msg ->
    model <# do
      replyText (pack $ show $ someFunc $ unpack msg)

runWebHook :: Text -> Port -> String -> IO ()
runWebHook token port ip = do
  env <- defaultTelegramClientEnv (Token token)
  res <- startBotWebhook bot config env
  print res
  where
    bot = conversationBot updateChatId pacalcBot
    tlsOpts = defaultTlsSettings {onInsecure = AllowInsecure}
    warpOpts = setPort port defaultSettings
    url = "https://" ++ ip ++ ":" ++ show port
    requestData =
      (defSetWebhook url)
        { setWebhookCertificate = Nothing,
          setWebhookAllowedUpdates = Just ["message"]
        }
    config =
      WebhookConfig
        { webhookConfigTlsSettings = tlsOpts,
          webhookConfigTlsWarpSettings = warpOpts,
          webhookConfigSetWebhookRequest = requestData
        }

runPolling :: Text -> IO ()
runPolling token = do
  env <- defaultTelegramClientEnv (Token token)
  startBot_ pacalcBot env


someFunc :: String -> Either ParseError [Answer]
someFunc line  = (findingsToQuestions >=> answer) <$> parseFindings line

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