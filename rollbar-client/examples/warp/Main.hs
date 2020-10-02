{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Rollbar.Client.Item as R

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Bool
import Debug.Trace
import Network.HTTP.Req
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings)
import Network.Wai.Parse
import Rollbar.Client
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
rollbarOnException settings mreq ex = do
  forkIO $ runRollbar defaultHttpConfig settings $ do
    rdata <- R.mkData $ R.PayloadTrace $ R.Trace [] $ R.mkExceptionFromSomeException ex
    rreq <- mapM mkRequest mreq
    let item = R.Item rdata { R.dataRequest = rreq }
    createItem item
    pure ()

  return ()
  where

mkRequest :: MonadIO m => Request -> m R.Request
mkRequest req = liftIO $ do
  (params, _) <- parseRequestBody ignoreBackEnd req
  body <- T.decodeUtf8 . BSL.toStrict <$> strictRequestBody req
  return R.Request
    { R.requestUrl = maybe "" toUrl $ requestHeaderHost req
    , R.requestMethod = T.decodeUtf8 $ requestMethod req
    , R.requestHeaders = HM.fromList $ fmap toHeader $ requestHeaders req
    , R.requestParams = mempty
    , R.requestGet = HM.fromList $ fmap toQuery $ queryString req
    , R.requestQueryStrings = T.decodeUtf8 $ renderQuery False $ queryString req
    , R.requestPost = HM.fromList $ fmap toParam params
    , R.requestBody = body
    , R.requestUserIp = ""
    }
  where
    toUrl host = T.decodeUtf8 $ mconcat
      -- TODO: guessAppRoot
      [ bool "http" "https" $ isSecure req
      , "://"
      , host
      , rawPathInfo req
      , rawQueryString req
      ]
    toHeader (key, value) =
      (T.decodeUtf8 $ CI.original key, toJSON $ T.decodeUtf8 value)
    toQuery (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 <$> value)
    toParam (key, value) =
      (T.decodeUtf8 key, toJSON $ T.decodeUtf8 value)

ignoreBackEnd :: BackEnd ()
ignoreBackEnd _ _ _ = return ()

app :: Application
app req respond =
  case rawPathInfo req of
    "/error" ->
      error "Zoom"
    _ ->
      respond $ responseLBS status404 [] "Not found"
