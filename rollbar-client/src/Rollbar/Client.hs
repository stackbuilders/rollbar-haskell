{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Replace String with Text
module Rollbar.Client where

import Network.HTTP.Req
import Rollbar.Client.Item

data Pong = Pong
  deriving (Eq, Show)

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
