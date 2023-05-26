{-# LANGUAGE OverloadedStrings #-}

module Env
  ( getConfig
  , getConfigByPath
  , Config (..)
  ) where

import System.FilePath ((</>))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import Data.Text (Text, pack)
import Data.Yaml

type Token = Text

newtype Config
    = Config { telegram :: Token }
    deriving Show

instance FromJSON Config where
  parseJSON = withObject "Config"
        $ \o -> Config
                <$> (pack <$> o .: "telegram-token")

configPath :: IO FilePath
configPath = fmap (</> "secrets.yaml") (getXdgDirectory XdgConfig "pacalcbot")

getConfig :: IO (Either ParseException Config)
getConfig = do
  decodeFileEither =<< configPath

getConfigByPath :: String -> IO (Either ParseException Config)
getConfigByPath = decodeFileEither

