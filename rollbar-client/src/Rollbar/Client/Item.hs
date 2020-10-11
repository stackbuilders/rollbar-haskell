{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollbar.Client.Item where

import qualified Control.Exception as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid (Endo(..))
import Data.Text
import Network.HTTP.Req
import Rollbar.Client.Internal
import Rollbar.Client.Settings
import System.Directory (getCurrentDirectory)
import System.Info (arch, os)

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

newtype Body = Body { bodyPayload :: Payload }
  deriving (Eq, Show)

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

-- | Pulls 'RequestModifiers' out of 'Settings' and build a list of 'Endo
-- Request' which are folded as a single request modifier function.
getRequestModifier :: (HasSettings m, Monad m) => m (Request -> Request)
getRequestModifier = do
  RequestModifiers{..} <- settingsRequestModifiers <$> getSettings
  return $ appEndo $ mconcat $ catMaybes
    [ withHeaders . excludeNames <$> requestModifiersExcludeHeaders
    , withParams . excludeNames <$> requestModifiersExcludeParams
    , withHeaders . includeNames <$> requestModifiersIncludeHeaders
    , withParams . includeNames <$> requestModifiersIncludeParams
    ]
  where
    withHeaders f = Endo $ \request -> request
      { requestHeaders = f $ requestHeaders request }
    withParams f = Endo $ \request -> request
      { requestParams = f $ requestParams request }
    excludeNames names = HM.filterWithKey $ \name _ -> name `notElem` names
    includeNames names = HM.filterWithKey $ \name _ -> name `elem` names

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

newtype ItemId = ItemId Text
  deriving (Eq, Show)

instance FromJSON ItemId where
  parseJSON = withObject "ItemId" $ \o ->
    ItemId <$> o .: "uuid"

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
