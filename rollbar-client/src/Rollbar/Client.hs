{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString
import Network.HTTP.Req
import Rollbar.Client.Item

data Pong = Pong
  deriving (Eq, Show)

newtype Token = Token ByteString
  deriving (Eq, Show)

data Response a = Response
  { responseErr :: Integer
  , responseResult :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "Response a" $ \o ->
    Response <$> o .: "err"
             <*> o .: "result"

newtype Rollbar a = Rollbar (ReaderT Token Req a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader Token
    )

instance MonadHttp Rollbar where
  handleHttpException = Rollbar . lift . handleHttpException

runWihToken :: Token -> Rollbar a -> IO a
runWihToken token (Rollbar f) = run $ runReaderT f token

run :: Req a -> IO a
run = runReq defaultHttpConfig

ping :: Req Pong
ping = do
  req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"

createItem :: Item -> Rollbar (Response ItemId)
createItem item = do
  Token token <- ask
  responseBody <$> req POST url (ReqBodyJson item) jsonResponse (opts token)
  where
    url = baseUrl /: "item" /: ""
    opts token = header "X-Rollbar-Access-Token" token

baseUrl :: Url Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
