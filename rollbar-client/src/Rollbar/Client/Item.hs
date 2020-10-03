{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client.Item where

import qualified Data.Text as T

import Control.Exception (SomeException, displayException)
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Rollbar.Client.Settings (Settings(..))
import System.Directory (getCurrentDirectory)
import System.Info (arch, os)

newtype Item = Item
  { itemData :: Data
  } deriving (Eq, Show)

instance ToJSON Item where
  toJSON Item{..} = object
    [ "data" .= itemData
    ]

data Data = Data
  { dataEnvironment :: Text
  , dataBody :: Body
  -- level
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
    , "platform" .= dataPlatform
    , "language" .= dataLanguage
    , "framework" .= dataFramework
    , "request" .= dataRequest
    , "server" .= dataServer
    , "notifier" .= dataNotifier
    ]

data Body = Body
  { -- telemetry
    bodyPayload :: Payload
  } deriving (Eq, Show)

instance ToJSON Body where
  toJSON Body{..} = object
    [ case bodyPayload of
        (PayloadTrace trace) -> ("trace", toJSON trace)
        (PayloadTraceChain traceChain) -> ("trace_chain", toJSON traceChain)
    ]

data Request = Request
  { requestUrl :: Text
  , requestMethod :: Text
  , requestHeaders :: Object
  , requestParams :: Object
  , requestGet :: Object
  , requestQueryStrings :: Text
  , requestPost :: Object
  , requestBody :: Text
  , requestUserIp :: Text
  } deriving (Eq, Show)

instance ToJSON Request where
  toJSON Request{..} = object
    [ "url" .= requestUrl
    , "method" .= requestMethod
    , "headers" .= requestHeaders
    , "params" .= requestParams
    , "GET" .= requestGet
    , "query_string" .= requestQueryStrings
    , "POST" .= requestPost
    , "body" .= requestBody
    , "user_ip" .= requestUserIp
    ]

data Payload
  = PayloadTrace Trace
  | PayloadTraceChain [Trace]
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

newtype ItemId = ItemId Text
  deriving (Eq, Show)

instance FromJSON ItemId where
  parseJSON = withObject "ItemId" $ \o ->
    ItemId <$> o .: "uuid"

mkItem :: (MonadIO m, MonadReader Settings m) => Payload -> m Item
mkItem payload = Item <$> mkData payload

mkData :: (MonadIO m, MonadReader Settings m) => Payload -> m Data
mkData payload = do
  env <- asks settingsEnvironment
  root <- liftIO getCurrentDirectory
  return Data
    { dataEnvironment = env
    , dataBody = Body
        { bodyPayload = payload
        }
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
    , dataNotifier = Just Notifier
        { notifierName = "rollbar-client"
        , notifierVersion = "0.1.0.0"
        }
    }

mkExceptionFromSomeException :: SomeException -> Exception
mkExceptionFromSomeException = mkException . T.pack . displayException

mkException :: Text -> Exception
mkException eclass= Exception
  { exceptionClass = eclass
  , exceptionMessage = Nothing
  , exceptionDescription = Nothing
  }
