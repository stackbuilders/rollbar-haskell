{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client.Common
  ( Settings(..)
  , Token(..)
  , Response(..)
  , readSettings
  ) where

import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Yaml
import Data.Yaml.Config

data Settings = Settings
  { settingsToken :: Token
  , settingsEnvironment :: Text
  } deriving (Eq, Show)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o ->
    Settings <$> o .: "token"
             <*> o .: "environment"

newtype Token = Token ByteString
  deriving (Eq, Show)

instance FromJSON Token where
  parseJSON = withText "Token" $ pure . Token . T.encodeUtf8

data Response a = Response
  { responseErr :: Integer
  , responseResult :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "Response a" $ \o ->
    Response <$> o .: "err"
             <*> o .: "result"

readSettings :: FilePath -> IO Settings
readSettings path = loadYamlSettings [path] [] requireEnv
