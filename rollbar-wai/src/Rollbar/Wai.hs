{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Wai
  ( rollbarOnException
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Network.Wai as W
import qualified Network.Wai.Parse as W

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Bool
import Network.HTTP.Req (defaultHttpConfig)
import Network.HTTP.Types (renderQuery)
import Rollbar.Client
import Rollbar.Client.Item
import Rollbar.Client.Settings

rollbarOnException :: Settings -> Maybe W.Request -> SomeException -> IO ()
rollbarOnException settings mreq ex = void $ forkIO $
  runRollbar defaultHttpConfig settings $ do
    rdata <- mkData $ PayloadTrace $ Trace [] $ mkExceptionFromSomeException ex
    rreq <- mapM mkRequest mreq
    let item = Item rdata { dataRequest = rreq }
    void $ createItem item

mkRequest :: MonadIO m => W.Request -> m Request
mkRequest req = liftIO $ do
  (params, _) <- W.parseRequestBody ignoreFilesBackEnd req
  body <- T.decodeUtf8 . BSL.toStrict <$> W.strictRequestBody req
  return Request
    { requestUrl = maybe "" toUrl $ W.requestHeaderHost req
    , requestMethod = T.decodeUtf8 $ W.requestMethod req
    , requestHeaders = HM.fromList $ fmap toHeader $ W.requestHeaders req
    , requestParams = mempty
    , requestGet = HM.fromList $ fmap toQuery $ W.queryString req
    , requestQueryStrings = T.decodeUtf8 $ renderQuery False $ W.queryString req
    , requestPost = HM.fromList $ fmap toParam params
    , requestBody = body
    , requestUserIp = ""
    }
  where
    toUrl host = T.decodeUtf8 $ mconcat
      -- TODO: guessAppRoot
      [ bool "http" "https" $ W.isSecure req
      , "://"
      , host
      , W.rawPathInfo req
      , W.rawQueryString req
      ]
    toHeader (key, value) =
      (T.decodeUtf8 $ CI.original key, toJSON $ T.decodeUtf8 value)
    toQuery (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 <$> value)
    toParam (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 value)

ignoreFilesBackEnd :: W.BackEnd ()
ignoreFilesBackEnd _ _ _ = pure ()
