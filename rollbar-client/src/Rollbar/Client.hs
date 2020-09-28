{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client where

import Data.Aeson
import Data.ByteString
import Network.HTTP.Req
import Rollbar.Client.Item

data Pong = Pong
  deriving (Eq, Show)

data Response a = Response
  { responseErr :: Integer
  , responseResult :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "Response a" $ \o ->
    Response <$> o .: "err"
             <*> o .: "result"

run :: Req a -> IO a
run = runReq defaultHttpConfig

ping :: Req Pong
ping = do
  req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"

createItem :: ByteString -> Item -> Req (Response ItemId)
createItem token item =
  responseBody <$> req POST url (ReqBodyJson item) jsonResponse opts
  where
    url = baseUrl /: "item" /: ""
    opts = header "X-Rollbar-Access-Token" token

baseUrl :: Url Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
