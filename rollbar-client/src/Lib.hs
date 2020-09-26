{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Replace String with Text
module Lib where

import Network.HTTP.Req

data Pong = Pong
  deriving (Eq, Show)

data Item = Item
  { itemData :: Data
  , itemEnvironment :: String
  , itemBody :: Body
  }

data Data = Data
  { dataBody :: Body
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
  { frameFilename :: String
  , frameLineno :: Maybe Integer
  , frameColno :: Maybe Integer
  , frameMethod :: Maybe String
  , frameCode :: Maybe String
  , frameClassName :: Maybe String
  , frameContext :: Maybe Context
  -- argspec
  -- varargspec
  -- keywordspec
  -- locals
  }

data Context = Context
  { contextPre :: [String]
  , contextPost :: [String]
  }

data Exception = Exception
  { exceptionClass :: String
  , exceptionMessage :: Maybe String
  , exceptionDescription :: Maybe String
  }

newtype ItemId = ItemId String

run :: Req a -> IO a
run = runReq defaultHttpConfig

ping :: Req Pong
ping = do
  req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"

createItem :: Item -> Req ItemId
createItem = undefined

baseUrl :: Url Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
