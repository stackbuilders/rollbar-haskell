{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client.Deploy
  ( -- ** Requests
    Deploy(..)
  , mkDeploy
  , Status(..)
    -- ** Responses
  , DeployId(..)
    -- ** Endpoints
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
    -- ^ Environment to which the revision was deployed.
  , deployRevision :: Revision
    -- ^ Git SHA of revision being deployed.
  , deployRollbarUsername :: Maybe Text
    -- ^ Rollbar username of person who deployed.
  , deployLocalUsername :: Maybe Text
    -- ^ Local username of person who deployed. Displayed in web app if no
    -- 'deployRollbarUsername' was specified.
  , deployComment :: Maybe Text
    -- ^ Additional text to include with the deploy.
  , deployStatus :: Maybe Status
    -- ^ Status of the deployment.
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

-- | Builds a 'Deploy' based on a 'Revision'.
--
-- __Example__
--
-- > getRevision >>= mkDeploy
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

-- | Status of the deployment.
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
-- __Example__
--
-- > settings <- readSettings "rollbar.yaml"
-- > runRollbar settings $ do
-- >   deploy <- getRevision >>= mkDeploy
-- >   reportDeploy deploy
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
