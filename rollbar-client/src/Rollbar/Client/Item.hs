{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client.Item where

import Data.Text

data Item = Item
  { itemData :: Data
  , itemEnvironment :: Text
  , itemBody :: Body
  }

data Data = Data
  { dataBody :: Body
  -- level
  -- timestamp
  -- code_version
  -- platform
  -- language
  -- framework
  -- context
  -- request
  -- person
  -- server
  -- client
  -- custom
  -- fingerprint
  -- title
  -- uuid
  , dataNotifier :: Maybe Notifier
  }

data Body = Body
  { bodyTelemetry :: Maybe Telemetry
  , bodyPayload :: Payload
  }

data Telemetry = Telemetry

data Payload
  = PayloadTrace Trace
  | PayloadTraceChain [Trace]

data Trace = Trace
  { traceFrames :: [Frame]
  , traceException :: Exception
  }

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
  }

data Context = Context
  { contextPre :: [Text]
  , contextPost :: [Text]
  }

data Exception = Exception
  { exceptionClass :: Text
  , exceptionMessage :: Maybe Text
  , exceptionDescription :: Maybe Text
  }

data Notifier = Notifier
  { notifierName :: Text
  , notifierVersion :: Text
  }

newtype ItemId = ItemId Text

defaultNotifier :: Notifier
defaultNotifier =
  Notifier
    { notifierName = "rollbar-client"
    , notifierVersion = "0.1.0.0"
    }
