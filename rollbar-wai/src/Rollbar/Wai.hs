{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Rollbar.Wai
-- Copyright: (c) 2020-present Stack Builders Inc.
-- License: MIT
-- Maintainer: Sebastián Estrella <sestrella@stackbuilders.com>
--
-- For a fully working example check the following link:
--
-- <https://github.com/stackbuilders/rollbar-haskell/blob/master/rollbar-wai/example/Main.hs>
module Rollbar.Wai
  ( rollbarOnException
  , rollbarOnExceptionWith
  , mkRequest
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text.Encoding as T
import qualified Network.Wai as W
import qualified Network.Wai.Parse as W
import qualified Network.Wai.Request as W

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Bifunctor (first)
import Network.HTTP.Types (renderQuery)
import Rollbar.Client

-- | Sends the captured 'SomeException' to Rollbar API using the given
-- 'Settings'. Under the hood, this function uses 'createItem' function from
-- rollbar-client.
--
-- __Example__
--
-- > settings <- readSettings "rollbar.yaml"
-- > runSettings
-- >   (setOnException (rollbarOnException settings) defaultSettings)
-- >   app
--
-- @since 0.1.0
rollbarOnException
  :: MonadIO m
  => Settings
  -> Maybe W.Request
  -> SomeException
  -> m ()
rollbarOnException = rollbarOnExceptionWith (void . createItem)

-- | Similar to 'rollbarOnExceptionWith', but it allows customize the function
-- used to send the 'Item' to Rollbar.
--
-- @since 0.1.0
rollbarOnExceptionWith
  :: MonadIO m
  => (Item -> Rollbar ())
  -> Settings
  -> Maybe W.Request
  -> SomeException
  -> m ()
rollbarOnExceptionWith f settings waiRequest ex =
  void $ liftIO $ forkIO $ runRollbar settings $ do
    item <- mkItem $ PayloadTrace $ Trace [] $ mkException ex
    request <- mapM mkRequest waiRequest
    f item
      { itemFramework = Just "wai"
      , itemRequest = request
      }

-- | Transforms a Wai 'W.Request' into a Rollbar 'Request'.
--
-- @since 0.1.0
mkRequest :: MonadIO m => W.Request -> m Request
mkRequest req = liftIO $ do
  (params, _) <- W.parseRequestBody ignoreFiles req
  return Request
    { requestUrl = T.decodeUtf8 $ mconcat
        [ W.guessApproot req
        , W.rawPathInfo req
        , W.rawQueryString req
        ]
    , requestMethod = T.decodeUtf8 $ W.requestMethod req
    , requestHeaders = KM.fromList $ map (first K.fromText) $ toHeader <$> W.requestHeaders req
    , requestParams = mempty
    , requestGet = KM.fromList $ map (first K.fromText) $ toQuery <$> W.queryString req
    , requestQueryStrings = T.decodeUtf8 $ renderQuery False $ W.queryString req
    , requestPost = KM.fromList $ map (first K.fromText) $ fmap toParam params
    , requestBody = ""
    , requestUserIp = ""
    }
  where
    toHeader (key, value) =
      (T.decodeUtf8 $ CI.original key, toJSON $ T.decodeUtf8 value)
    toQuery (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 <$> value)
    toParam (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 value)

ignoreFiles :: W.BackEnd ()
ignoreFiles _ _ _ = pure ()
