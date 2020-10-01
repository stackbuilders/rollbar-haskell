{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Rollbar.Client.Item as R

import Control.Exception
import Data.Aeson
import Data.Bool
import Debug.Trace
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings)
import Rollbar.Client.Settings
import System.Environment

main :: IO ()
main = do
  settings <- getSettings
  let warpSettings = setOnException (rollbarOnException settings) defaultSettings
  runSettings warpSettings app

getSettings :: IO Settings
getSettings =
  Settings <$> BS.pack <$> getEnv "ROLLBAR_TOKEN"
           <*> pure "test"

rollbarOnException :: Settings -> Maybe Request -> SomeException -> IO ()
rollbarOnException _ mreq _ = do
  print mreq
  print $ fmap toRollbarRequest mreq
  return ()

toRollbarRequest :: Request -> R.Request
toRollbarRequest req = R.Request
  { R.requestUrl = maybe "" fullUrl $ requestHeaderHost req
  , R.requestMethod = T.decodeUtf8 $ requestMethod req
  , R.requestHeaders = HM.fromList $ fmap toHeader $ requestHeaders req
  , R.requestParams = mempty
  , R.requestGet = HM.fromList []
  , R.requestQueryStrings = T.decodeUtf8 $ renderQuery False $ queryString req
  , R.requestPost = mempty
  , R.requestBody = mempty
  , R.requestUserIp = ""
  }
  where
    fullUrl host = T.decodeUtf8 $ mconcat
      [ bool "http" "https" (isSecure req)
      , "://"
      , host
      , rawPathInfo req
      , rawQueryString req
      ]
    toHeader (key, value) = (T.decodeUtf8 $ CI.original key, toJSON $ T.decodeUtf8 value)

app :: Application
app req respond =
  case rawPathInfo req of
    "/error" -> do
      error "Boom"
    _ ->
      respond $ responseLBS status404 [] "Not found"
