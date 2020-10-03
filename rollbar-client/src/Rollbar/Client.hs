{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client
  ( module R
  , Rollbar (..)
  , Pong (..)
  , withRollbar
  , runRollbar
  , ping
  , createItem
  , reportDeploy
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Network.HTTP.Req
import Rollbar.Client.Common as R
import Rollbar.Client.Deploy as R
import Rollbar.Client.Item as R

newtype Rollbar a = Rollbar (ReaderT Settings Req a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader Settings
    )

instance MonadHttp Rollbar where
  handleHttpException = Rollbar . lift . handleHttpException

data Pong = Pong
  deriving (Eq, Show)

withRollbar :: (MonadCatch m, MonadIO m) => Settings -> m a -> m a
withRollbar settings f = f `catch` handleException settings

handleException
  :: (MonadIO m, MonadThrow m)
  => Settings
  -> SomeException
  -> m a
handleException settings ex = do
  void $ runRollbar settings $ do
    item <- mkItem $ PayloadTrace $ Trace [] $ mkExceptionFromSomeException ex
    createItem item

  throwM ex

runRollbar :: MonadIO m => Settings -> Rollbar a -> m a
runRollbar settings (Rollbar f) = runReq defaultHttpConfig $ runReaderT f settings

-- | https://explorer.docs.rollbar.com/#section/Ping
ping :: MonadHttp m => m Pong
ping = do
  void $ req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"

-- | https://explorer.docs.rollbar.com/#operation/create-item
createItem
  :: (MonadHttp m, MonadReader Settings m)
  => Item
  -> m ItemId
createItem item =
  responseResult . responseBody <$> rollbar POST url (ReqBodyJson item) jsonResponse mempty
  where
    url = baseUrl /: "item" /: ""

-- | https://explorer.docs.rollbar.com/#operation/post-deploy
reportDeploy
  :: (MonadHttp m, MonadReader Settings m)
  => Deploy
  -> m DeployId
reportDeploy deploy =
  unDataResponse . responseBody <$> rollbar POST url (ReqBodyJson deploy) jsonResponse mempty
  where
    url = baseUrl /: "deploy"

rollbar
  :: ( HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , HttpMethod method
     , HttpResponse response
     , MonadHttp m
     , MonadReader Settings m
     )
  => method
  -> Url 'Https
  -> body
  -> Proxy response
  -> Option 'Https
  -> m response
rollbar method url body response options = do
  Token token <- asks settingsToken
  req method url body response $ options <> header "X-Rollbar-Access-Token" token

baseUrl :: Url 'Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
