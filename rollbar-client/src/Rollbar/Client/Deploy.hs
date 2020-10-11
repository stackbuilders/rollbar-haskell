{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client.Deploy
  ( Deploy(..)
  , mkDeploy
  , Status(..)
  , DeployId(..)
  , reportDeploy
  ) where

import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Text
import Network.HTTP.Req
import Rollbar.Client.Internal
import Rollbar.Client.Settings
import System.Environment (lookupEnv)

data Deploy = Deploy
  { deployEnvironment :: Environment
  , deployRevision :: Revision
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

mkDeploy
  :: (HasSettings m, MonadIO m)
  => Revision
  -> m Deploy
mkDeploy revision = do
  env <- settingsEnvironment <$> getSettings
  muser <- fmap T.pack <$> liftIO (lookupEnv "USER")
  return Deploy
    { deployEnvironment = env
    , deployRevision = revision
    , deployRollbarUsername = Nothing
    , deployLocalUsername = muser
    , deployComment = Nothing
    , deployStatus = Just StatusSucceeded
    }

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

newtype DeployId = DeployId Integer
  deriving (Eq, Num, Ord, Show)

instance FromJSON DeployId where
  parseJSON = withObject "DeployId" $ \o ->
    DeployId <$> o .: "deploy_id"

-- | Tracks a deploy in Rollbar.
--
-- __Reference__
--
-- <https://explorer.docs.rollbar.com/#operation/post-deploy>
reportDeploy
  :: (HasSettings m, MonadHttp m)
  => Deploy
  -> m DeployId
reportDeploy deploy =
  fmap
    (unDataResponse . responseBody)
    (rollbar POST url (ReqBodyJson deploy) jsonResponse mempty)
  where
    url = baseUrl /: "deploy"
