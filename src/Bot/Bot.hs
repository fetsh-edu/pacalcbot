{-# LANGUAGE OverloadedStrings #-}
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
    defaultTlsSettings
  )
import Telegram.Bot.API
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (InputMessageContent(..))
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser ( updateMessageText)
import Calculator.Types
import Data.List (nub)
import Calculator.Calculator (answer)
import Parser.Finding (Finding(..), parseFindings)
import Control.Monad ((>=>))
import Text.Parsec (ParseError)
import Telegram.Bot.Simple.Debug (traceBotActionsShow, traceTelegramUpdatesWith)
import qualified Data.Aeson as Aeson (encode)
import Data.Aeson (ToJSON)
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Encoding    as Text
import Data.Either (fromRight)


type Model = ()

data Action
  = InlineIncoming InlineQueryId Text
  | Incoming Text
  deriving (Show)

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
        let 
            results = either (const []) (fmap (answerToArticle msg)) $ buildAnswers (unpack msg)
            answerInlineQueryRequest = defAnswerInlineQuery queryId results
        _ <- runTG answerInlineQueryRequest
        return ()
  Incoming msg ->
    model <# do
      replyText (fromRight "Не получилось понять вопрос" $ showAnswers <$> buildAnswers (unpack msg))

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
          setWebhookAllowedUpdates = Just ["message", "edited_message", "inline_query"]
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
  let conversationBot_ = (traceTelegramUpdatesUglyJSON . traceBotActionsShow) pacalcBot
  startBot_ conversationBot_ env

upAsJSON :: ToJSON a => a -> String
upAsJSON = LText.unpack . Text.decodeUtf8 . Aeson.encode

traceTelegramUpdatesUglyJSON :: BotApp model action -> BotApp model action
traceTelegramUpdatesUglyJSON = traceTelegramUpdatesWith upAsJSON

buildAnswers :: String -> Either ParseError [Answer]
buildAnswers line  = (findingsToQuestions >=> answer) <$> parseFindings line

showAnswers :: [Answer] -> Text
showAnswers [] = "Не получилось найти ответ"
showAnswers xs = Text.intercalate "\n" $ fmap showAnswer xs

showAnswer :: Answer -> Text
showAnswer (Answer dist time pace) = pack $ show dist <> " [" <> show pace <> "]: " <> show time

answerToArticle :: Text -> Answer -> InlineQueryResult
answerToArticle msg answer_ = defInlineQueryResultArticle thumbnail
    where
        answerText = showAnswer answer_
        thumbnail = (defInlineQueryResultGenericThumbnail result)
                      { inlineQueryResultGenericThumbnailWidth = Just 100
                      , inlineQueryResultGenericThumbnailHeight = Just 100
                      , inlineQueryResultGenericThumbnailUrl = Just "https://fetsh.me/img/pace.png"
                      }
        result = (defInlineQueryResultGeneric (InlineQueryResultId (head $ Text.splitOn "]" answerText )))
                  { inlineQueryResultTitle = Just msg
                  , inlineQueryResultDescription = Just answerText
                  , inlineQueryResultInputMessageContent = Just (InputTextMessageContent (msg <> " (<b>" <> answerText <> "</b>)") (Just "HTML") (Just True) )
                  }

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