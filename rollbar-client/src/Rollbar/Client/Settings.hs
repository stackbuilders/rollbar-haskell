{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client.Settings
  ( HasSettings(..)
  , Settings(..)
  , readSettings
  , Token(..)
  , Environment(..)
  , Revision(..)
  , getRevision
  , RequestModifiers(..)
  , defaultRequestModifiers
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import System.Process

class HasSettings m where
  getSettings :: m Settings

data Settings = Settings
  { settingsToken :: Token
  , settingsEnvironment :: Environment
  , settingsRevision :: Maybe Revision
  , settingsRequestModifiers :: RequestModifiers
  } deriving (Eq, Show)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \o ->
    Settings <$> o .: "token"
             <*> o .: "environment"
             <*> o .:? "revision" .!= Nothing
             <*> o .:? "request_modifiers" .!= defaultRequestModifiers

-- | Reads 'Settings' from a YAML file.
readSettings :: MonadIO m => FilePath -> m Settings
readSettings path = liftIO $ loadYamlSettings [path] [] requireEnv

newtype Token = Token ByteString
  deriving (Eq, Show)

instance FromJSON Token where
  parseJSON = withText "Token" $ pure . Token . T.encodeUtf8

newtype Environment = Environment Text
  deriving (Eq, FromJSON, Show, ToJSON)

newtype Revision = Revision Text
  deriving (Eq, FromJSON, Show, ToJSON)

getRevision
  :: (HasSettings m, MonadIO m)
  => m Revision
getRevision = do
  mrevision <- settingsRevision <$> getSettings
  case mrevision of
    Nothing ->
      mkRevision <$> liftIO (readProcess "git" ["rev-parse", "HEAD"] "")
    Just revision -> return revision
  where
    mkRevision = Revision . T.stripEnd . T.pack

data RequestModifiers = RequestModifiers
  { requestModifiersExcludeHeaders :: Maybe (NonEmpty Text)
  , requestModifiersExcludeParams :: Maybe (NonEmpty Text)
  , requestModifiersIncludeHeaders :: Maybe (NonEmpty Text)
  , requestModifiersIncludeParams :: Maybe (NonEmpty Text)
  } deriving (Eq, Show)

instance FromJSON RequestModifiers where
  parseJSON = withObject "RequestModifiers" $ \o ->
    RequestModifiers <$> o .:? "exclude_headers" .!= Nothing
                     <*> o .:? "exclude_params" .!= Nothing
                     <*> o .:? "include_headers" .!= Nothing
                     <*> o .:? "include_params" .!= Nothing

-- | Returns an empty 'RequestModifiers', the function produced by
-- 'getRequestModifier' given this values is equivalent to 'id'.
defaultRequestModifiers :: RequestModifiers
defaultRequestModifiers = RequestModifiers
  { requestModifiersExcludeHeaders = Nothing
  , requestModifiersExcludeParams = Nothing
  , requestModifiersIncludeHeaders = Nothing
  , requestModifiersIncludeParams = Nothing
  }
