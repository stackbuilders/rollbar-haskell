{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client
  (
  -- * Settings
    module Rollbar.Client.Settings
  -- ** Types
  , Rollbar
  -- ** Top Functions
  , withRollbar
  , runRollbar
  -- * Ping
  -- ** Responses
  , Pong(..)
  -- ** Endpoints
  , ping
  -- * Item
  -- ** Requests
  , Item(..)
  , mkItem
  , Data(..)
  , mkData
  , Body(..)
  , Payload(..)
  , Trace(..)
  , Frame(..)
  , Exception(..)
  , mkException
  , Request(..)
  , Message(..)
  , Server(..)
  , Notifier(..)
  , defaultNotifier
  -- ** Responses
  , ItemId(..)
  -- ** Endpoints
  , createItem
  -- * Deploy
  -- ** Requests
  , Deploy(..)
  , mkDeploy
  -- ** Responses
  , DeployId(..)
  -- ** Endpoints
  , reportDeploy
  ) where

import qualified Control.Exception as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Yaml.Config
import Network.HTTP.Req
import Rollbar.Client.Settings
import System.Directory
import System.Environment
import System.Info (arch, os)
import System.Process

newtype Rollbar a = Rollbar (ReaderT Settings Req a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    )

instance MonadHttp Rollbar where
  handleHttpException = Rollbar . lift . handleHttpException

instance HasSettings Rollbar where
  getSettings = Rollbar ask

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

newtype DataResponse a = DataResponse { unDataResponse :: a }
  deriving (Eq, Show)

instance FromJSON a => FromJSON (DataResponse a) where
  parseJSON = withObject "DataResponse" $ \o ->
    DataResponse <$> o .: "data"

data ResultResponse a = ResultResponse
  { resultResponseErr :: Integer
  , resultResponseResult :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (ResultResponse a) where
  parseJSON = withObject "ResultResponse" $ \o ->
    ResultResponse <$> o .: "err"
                   <*> o .: "result"

--------------------------------------------------------------------------------
-- Common / Top Functions
--------------------------------------------------------------------------------

-- | Runs a computation, captures any 'E.SomeException' threw, and send it to
-- Rollbar.
withRollbar :: (MonadCatch m, MonadIO m) => Settings -> m a -> m a
withRollbar settings f = f `catch` \ex -> do
  void $ runRollbar settings $ do
    item <- mkItem $ PayloadTrace $ Trace [] $ mkException ex
    createItem item

  throwM ex

-- | Run a computation in 'Rollbar' monad.
runRollbar :: MonadIO m => Settings -> Rollbar a -> m a
runRollbar settings (Rollbar f) = runReq defaultHttpConfig $
  runReaderT f settings

--------------------------------------------------------------------------------
-- Ping
--
-- $ping
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Ping / Response
--------------------------------------------------------------------------------

data Pong = Pong
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Ping / Endpoint
--------------------------------------------------------------------------------

-- | Pings Rollbar API server, on success returns 'Pong'.
--
-- __Reference__
--
-- <https://explorer.docs.rollbar.com/#section/Ping>
ping :: MonadHttp m => m Pong
ping = do
  void $ req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"

--------------------------------------------------------------------------------
-- Item
--
-- $item
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Item / Request
--------------------------------------------------------------------------------

newtype Item = Item
  { itemData :: Data
  } deriving (Eq, Show)

instance ToJSON Item where
  toJSON Item{..} = object
    [ "data" .= itemData
    ]

mkItem
  :: (HasSettings m, MonadIO m)
  => Payload
  -> m Item
mkItem payload = Item <$> mkData payload

data Data = Data
  { dataEnvironment :: Environment
  , dataBody :: Body
  , dataLevel :: Maybe Level
  -- timestamp
  -- code_version
  , dataPlatform :: Maybe Text
  , dataLanguage :: Maybe Text
  , dataFramework :: Maybe Text
  -- context
  -- request
  , dataRequest :: Maybe Request
  -- person
  , dataServer :: Maybe Server
  -- client
  -- custom
  -- fingerprint
  -- title
  -- uuid
  , dataNotifier :: Maybe Notifier
  } deriving (Eq, Show)

instance ToJSON Data where
  toJSON Data{..} = object
    [ "environment" .= dataEnvironment
    , "body" .= dataBody
    , "level" .= dataLevel
    , "platform" .= dataPlatform
    , "language" .= dataLanguage
    , "framework" .= dataFramework
    , "request" .= dataRequest
    , "server" .= dataServer
    , "notifier" .= dataNotifier
    ]

mkData
  :: (HasSettings m, MonadIO m)
  => Payload
  -> m Data
mkData payload = do
  environment <- settingsEnvironment <$> getSettings
  root <- liftIO getCurrentDirectory
  return Data
    { dataEnvironment = environment
    , dataBody = Body
        { bodyPayload = payload
        }
    , dataLevel = Just $ mkLevel payload
    , dataPlatform = Just $ T.pack os
    , dataLanguage = Just "haskell"
    , dataFramework = Nothing
    , dataRequest = Nothing
    , dataServer = Just Server
        { serverCpu = Just $ T.pack arch
        , serverHost = Nothing
        , serverRoot = Just $ T.pack root
        , serverBranch = Nothing
        , serverCodeVersion = Nothing
        }
    , dataNotifier = Just defaultNotifier
    }

newtype Body = Body
  { -- telemetry
    bodyPayload :: Payload
  } deriving (Eq, Show)

instance ToJSON Body where
  toJSON Body{..} = object
    [ case bodyPayload of
        (PayloadTrace trace) -> ("trace", toJSON trace)
        (PayloadTraceChain traceChain) -> ("trace_chain", toJSON traceChain)
        (PayloadMessage message) -> ("message", toJSON message)
    ]

data Payload
  = PayloadTrace Trace
  | PayloadTraceChain [Trace]
  | PayloadMessage Message
  deriving (Eq, Show)

data Trace = Trace
  { traceFrames :: [Frame]
  , traceException :: Exception
  } deriving (Eq, Show)

instance ToJSON Trace where
  toJSON Trace{..} = object
    [ "frames" .= traceFrames
    , "exception" .= traceException
    ]

data Frame = Frame
  { frameFilename :: Text
  , frameLineno :: Maybe Integer
  , frameColno :: Maybe Integer
  , frameMethod :: Maybe Text
  , frameCode :: Maybe Text
  , frameClassName :: Maybe Text
  , frameContext :: Maybe Context
  -- argspec
  -- varargspec
  -- keywordspec
  -- locals
  } deriving (Eq, Show)

instance ToJSON Frame where
  toJSON Frame{..} = object
    [ "filename" .= frameFilename
    , "lineno" .= frameLineno
    , "colno" .= frameColno
    , "method" .= frameMethod
    , "code" .= frameCode
    , "class_name" .= frameClassName
    , "context" .= frameContext
    ]

data Context = Context
  { contextPre :: [Text]
  , contextPost :: [Text]
  } deriving (Eq, Show)


instance ToJSON Context where
  toJSON Context{..} = object
    [ "pre" .= contextPre
    , "post" .= contextPost
    ]

data Exception = Exception
  { exceptionClass :: Text
  , exceptionMessage :: Maybe Text
  , exceptionDescription :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON Exception where
  toJSON Exception{..} = object
    [ "class" .= exceptionClass
    , "message" .= exceptionMessage
    , "description" .= exceptionDescription
    ]

-- | Builds a Rollbar 'Exception' based on 'E.SomeException'.
mkException :: E.SomeException -> Exception
mkException ex = Exception
  { exceptionClass = T.pack $ E.displayException ex
  , exceptionMessage = Nothing
  , exceptionDescription = Nothing
  }

data Message = Message
  { messageBody :: Text
  , messageMetadata :: Object
  } deriving (Eq, Show)

instance ToJSON Message where
  toJSON Message{..} = Object $
    HM.insert "body" (toJSON messageBody) messageMetadata

data Level
  = LevelCritical
  | LevelError
  | LevelWarning
  | LevelInfo
  | LevelDebug
  deriving (Eq, Show)

instance ToJSON Level where
  toJSON = \case
    LevelCritical -> "critical"
    LevelError -> "error"
    LevelWarning -> "warning"
    LevelInfo -> "info"
    LevelDebug -> "debug"

mkLevel :: Payload -> Level
mkLevel (PayloadMessage _) = LevelInfo
mkLevel _ = LevelError

data Server = Server
  { serverCpu :: Maybe Text
  , serverHost :: Maybe Text
  , serverRoot :: Maybe Text
  , serverBranch :: Maybe Text
  , serverCodeVersion :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON Server where
  toJSON Server{..} = object
    [ "cpu" .= serverCpu
    , "host" .= serverHost
    , "root" .= serverRoot
    , "branch" .= serverBranch
    , "code_version" .= serverCodeVersion
    ]

data Notifier = Notifier
  { notifierName :: Text
  , notifierVersion :: Text
  } deriving (Eq, Show)

instance ToJSON Notifier where
  toJSON Notifier{..} = object
    [ "name" .= notifierName
    , "version" .= notifierVersion
    ]

-- | Returns information about this package such as name and version.
defaultNotifier :: Notifier
defaultNotifier = Notifier
  { notifierName = "rollbar-client"
  , notifierVersion = "0.1.0"
  }

--------------------------------------------------------------------------------
-- Item / Response
--------------------------------------------------------------------------------

newtype ItemId = ItemId Text
  deriving (Eq, Show)

instance FromJSON ItemId where
  parseJSON = withObject "ItemId" $ \o ->
    ItemId <$> o .: "uuid"

--------------------------------------------------------------------------------
-- Item / Endpoints
--------------------------------------------------------------------------------

-- | Reports an occurrence (exception or message) to Rollbar.
--
-- __Reference__
--
-- <https://explorer.docs.rollbar.com/#operation/create-item>
createItem
  :: (HasSettings m, MonadHttp m)
  => Item
  -> m ItemId
createItem (Item itemData) = do
  requestModifier <- getRequestModifier
  fmap
    (resultResponseResult . responseBody)
    (rollbar POST url (body requestModifier) jsonResponse mempty)
  where
    url = baseUrl /: "item" /: ""
    body requestModifier = ReqBodyJson $ Item
      itemData { dataRequest = requestModifier <$> dataRequest itemData }

--------------------------------------------------------------------------------
-- Deploy
--
-- $deploy
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Deploy / Request
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Deploy / Response
--------------------------------------------------------------------------------

newtype DeployId = DeployId Integer
  deriving (Eq, Num, Ord, Show)

instance FromJSON DeployId where
  parseJSON = withObject "DeployId" $ \o ->
    DeployId <$> o .: "deploy_id"

--------------------------------------------------------------------------------
-- Deploy / Endpoints
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

rollbar
  :: ( HasSettings m
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , HttpMethod method
     , HttpResponse response
     , MonadHttp m
     )
  => method
  -> Url 'Https
  -> body
  -> Proxy response
  -> Option 'Https
  -> m response
rollbar method url body response options = do
  Token token <- settingsToken <$> getSettings
  req method url body response $ options <> header "X-Rollbar-Access-Token" token

baseUrl :: Url 'Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
