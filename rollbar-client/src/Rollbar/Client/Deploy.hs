{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client.Deploy where

import qualified Data.Text as T

import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Rollbar.Client.Settings (Settings(..))
import System.Environment
import System.Process

data Deploy = Deploy
  { deployEnvironment :: Text
  , deployRevision :: Text
  , deployRollbarUsername :: Maybe Text
  , deployLocalUsername :: Maybe Text
  , deployComment :: Maybe Text
  , deployStatus :: Maybe Status
  } deriving (Eq, Show)

instance ToJSON Deploy where
  toJSON Deploy{..} = object
    [ "environment" .= deployEnvironment
    , "revision" .= deployRevision
    , "rollbar_username" .= deployRollbarUsername
    , "local_username" .= deployLocalUsername
    , "comment" .= deployComment
    , "status" .= deployStatus
    ]

data Status
  = StatusStarted
  | StatusSucceeded
  | StatusFailed
  | StatusTimedOut
  deriving (Eq, Show)

instance ToJSON Status where
  toJSON StatusStarted = String "started"
  toJSON StatusSucceeded = String "succeeded"
  toJSON StatusFailed = String "failed"
  toJSON StatusTimedOut = String "timed_out"

newtype DataResponse a = DataResponse a
  deriving (Eq, Show)

instance FromJSON a => FromJSON (DataResponse a) where
  parseJSON = withObject "DataResponse" $ \o ->
    DataResponse <$> o .: "data"

newtype DeployId = DeployId Integer
  deriving (Eq, Num, Ord, Show)

instance FromJSON DeployId where
  parseJSON = withObject "DeployId" $ \o ->
    DeployId <$> o .: "deploy_id"

unDataResponse :: DataResponse a -> a
unDataResponse (DataResponse a) = a

mkDeploy :: (MonadIO m, MonadReader Settings m) => m Deploy
mkDeploy = do
  env <- asks settingsEnvironment
  revision <- T.stripEnd . T.pack <$> liftIO (readProcess "git" ["rev-parse", "HEAD"] "")
  muser <- fmap T.pack <$> liftIO (lookupEnv "USER")
  return Deploy
    { deployEnvironment = env
    , deployRevision = revision
    , deployRollbarUsername = Nothing
    , deployLocalUsername = muser
    , deployComment = Nothing
    , deployStatus = Just StatusSucceeded
    }
