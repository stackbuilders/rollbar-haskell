{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Rollbar.Client.Item
Copyright: (c) 2020 Stack Builders Inc.
License: MIT
Maintainer: Sebastián Estrella <sestrella@stackbuilders.com>

Most of the documentation in this module comes from Rollbar's official
documentation.

<https://explorer.docs.rollbar.com/#operation/create-item>
-}

module Rollbar.Client.Item
  ( -- * Requests
    Item(..)
  , mkItem
  , Data(..)
  , mkData
  , Body(..)
  , Payload(..)
  , Trace(..)
  , Frame(..)
  , Context(..)
  , Exception(..)
  , mkException
  , Message(..)
  , Level(..)
  , mkLevel
  , Request(..)
  , getRequestModifier
  , Server(..)
  , Notifier(..)
  , defaultNotifier
    -- * Responses
  , ItemId(..)
    -- * Endpoints
  , createItem
  ) where

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

-- | Builds an 'Item' based on a 'Payload'.
mkItem
  :: (HasSettings m, MonadIO m)
  => Payload
  -> m Item
mkItem payload = Item <$> mkData payload

data Data = Data
  { dataEnvironment :: Environment
    -- ^ The name of the environment in which this occurrence was seen. A
    -- string up to 255 characters. For best results, use "production" or
    -- "prod" for your production environment.  You don't need to configure
    -- anything in the Rollbar UI for new environment names; we'll detect them
    -- automatically.
  , dataBody :: Body
    -- ^ The main data being sent. It can either be a message, an exception, or
    -- a crash report.
  , dataLevel :: Maybe Level
    -- ^ The severity level. One of: "critical", "error", "warning", "info",
    -- "debug" Defaults to "error" for exceptions and "info" for messages.  The
    -- level of the *first* occurrence of an item is used as the item's level.
  -- timestamp
  -- code_version
  , dataPlatform :: Maybe Text
    -- ^ The platform on which this occurred. Meaningful platform names:
    -- "browser", "android", "ios", "flash", "client", "heroku",
    -- "google-app-engine" If this is a client-side event, be sure to specify
    -- the platform and use a post_client_item access token.
  , dataLanguage :: Maybe Text
    -- ^ The name of the language your code is written in.  This can affect the
    -- order of the frames in the stack trace. The following languages set the
    -- most recent call first - 'ruby', 'javascript', 'php', 'java',
    -- 'objective-c', 'lua' It will also change the way the individual frames
    -- are displayed, with what is most consistent with users of the language.
  , dataFramework :: Maybe Text
    -- ^ The name of the framework your code uses.
  -- context
  -- request
  , dataRequest :: Maybe Request
    -- ^ Data about the request this event occurred in.
  -- person
  , dataServer :: Maybe Server
    -- ^ Data about the server related to this event.
  -- client
  -- custom
  -- fingerprint
  -- title
  -- uuid
  , dataNotifier :: Notifier
    -- ^ Describes the library used to send this event.
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

-- | Builds an 'Data' based on a 'Payload'.
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
    , dataNotifier = defaultNotifier
    }

-- | The main data being sent. It can either be a message, an exception, or a
-- crash report.
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
    -- ^ A list of stack frames, ordered such that the most recent call is last
    -- in the list.
  , traceException :: Exception
  } deriving (Eq, Show)

instance ToJSON Trace where
  toJSON Trace{..} = object
    [ "frames" .= traceFrames
    , "exception" .= traceException
    ]

data Frame = Frame
  { frameFilename :: Text
    -- ^ The filename including its full path.
  , frameLineno :: Maybe Integer
    -- ^ The line number as an integer.
  , frameColno :: Maybe Integer
    -- ^ The column number as an integer.
  , frameMethod :: Maybe Text
    -- ^ The method or function name.
  , frameCode :: Maybe Text
    -- ^ The line of code.
  , frameClassName :: Maybe Text
    -- ^ A string containing the class name.  Used in the UI when the payload's
    -- top-level "language" key has the value "java".
  , frameContext :: Maybe Context
    -- ^ Additional code before and after the "code" line.
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

-- | Additional code before and after the "code" line.
data Context = Context
  { contextPre :: [Text]
    -- ^ List of lines of code before the "code" line.
  , contextPost :: [Text]
    -- ^ List of lines of code after the "code" line.
  } deriving (Eq, Show)

instance ToJSON Context where
  toJSON Context{..} = object
    [ "pre" .= contextPre
    , "post" .= contextPost
    ]

-- | An object describing the exception instance.
data Exception = Exception
  { exceptionClass :: Text
    -- ^ The exception class name.
  , exceptionMessage :: Maybe Text
    -- ^ The exception message, as a string.
  , exceptionDescription :: Maybe Text
    -- ^ An alternate human-readable string describing the exception Usually
    -- the original exception message will have been machine-generated; you can
    -- use this to send something custom.
  } deriving (Eq, Show)

instance ToJSON Exception where
  toJSON Exception{..} = object
    [ "class" .= exceptionClass
    , "message" .= exceptionMessage
    , "description" .= exceptionDescription
    ]

-- | Builds a 'Exception' based on 'E.SomeException'.
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

-- | The severity level. One of: "critical", "error", "warning", "info",
-- "debug" Defaults to "error" for exceptions and "info" for messages. The
-- level of the *first* occurrence of an item is used as the item's level.
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

-- | Builds a 'Level' based on a 'Payload'.
mkLevel :: Payload -> Level
mkLevel (PayloadMessage _) = LevelInfo
mkLevel _ = LevelError

-- | Data about the request this event occurred in.
data Request = Request
  { requestUrl :: Text
    -- ^ Full URL where this event occurred.
  , requestMethod :: Text
    -- ^ The request method.
  , requestHeaders :: Object
    -- ^ Object containing the request headers.
  , requestParams :: Object
    -- ^ Any routing parameters (i.e. for use with Rails Routes).
  , requestGet :: Object
    -- ^ Query string params.
  , requestQueryStrings :: Text
    -- ^ The raw query string.
  , requestPost :: Object
    -- ^ POST params.
  , requestBody :: Text
    -- ^ The raw POST body.
  , requestUserIp :: Text
    -- ^ Can also be the special value "$remote_ip", which will be replaced
    -- with the source IP of the API request.  Will be indexed, as long as it
    -- is a valid IPv4 address.
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
-- __Example__
--
-- > settings <- readSettings "rollbar.yaml"
-- > runRollbar settings $ do
-- >   item <- mkItem $ PayloadTrace $ Trace [] $ Exception
-- >     { exceptionClass = "NameError"
-- >     , exceptionMessage = Just "global name 'foo' is not defined"
-- >     , exceptionDescription = Just "Something went wrong while trying to save the user object"
-- >     }
-- >   createItem item
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
