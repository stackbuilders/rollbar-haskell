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

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Yaml.Config (loadYamlSettings, requireEnv)
import System.Process

-- | Typeclass used to pull Rollbar 'Settings' out of a given 'Monad'.
class HasSettings m where
  getSettings :: m Settings

-- | General settings required to interact with Rollbar API.
data Settings = Settings
  { settingsToken :: Token
    -- ^ Rollbar API authentication token.
  , settingsEnvironment :: Environment
    -- ^ Environment to which the revision was deployed.
  , settingsRevision :: Maybe Revision
    -- ^ Git SHA of revision being deployed.
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

-- | Environment to which the revision was deployed.
newtype Environment = Environment Text
  deriving (Eq, FromJSON, Show, ToJSON)

-- | Git SHA of revision being deployed.
newtype Revision = Revision Text
  deriving (Eq, FromJSON, Show, ToJSON)

-- | Gets the 'Revision' from 'Settings' if the value is present (not Nothing),
-- otherwise pulls the 'Revision' from @git@ directly running the following
-- command @git rev-parse HEAD@.
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

-- | Represents a list of 'Request' modifiers that are combined by
-- 'getRequestModifier' into a single function.
data RequestModifiers = RequestModifiers
  { requestModifiersExcludeHeaders :: Maybe (NonEmpty Text)
    -- ^ A list of 'Request' header names to be excluded.
  , requestModifiersExcludeParams :: Maybe (NonEmpty Text)
    -- ^ A list of 'Request' param names to be excluded.
  , requestModifiersIncludeHeaders :: Maybe (NonEmpty Text)
    -- ^ A list of 'Request' header names to be included.
  , requestModifiersIncludeParams :: Maybe (NonEmpty Text)
    -- ^ A list of 'Request' params names to be included.
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
