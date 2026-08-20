{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.WaiSpec
  ( spec
  ) where

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Network.HTTP.Types (status200, status404)
import Rollbar.Client
import Rollbar.Wai (rollbarOnExceptionWith)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = before getSettingsAndItemVar $
  describe "rollbarOnExceptionWith" $ do
    context "when the response status code is 200" $
      it "does not trigger a call to Rollbar" $
        withApp $ \itemVar warpPort -> do
          let url = http "localhost" /: "success"
          response <- runReq
            defaultHttpConfig
            (req GET url NoReqBody bsResponse $ port warpPort)
          responseStatusCode response `shouldBe` 200
          responseBody response `shouldBe` "OK"
          threadDelay 100_000
          tryReadMVar itemVar `shouldReturn` Nothing

    context "when the response status code is not 200" $
      it "triggers a call to Rollbar" $
        withApp $ \itemVar warpPort -> do
          let url = http "localhost" /: "error"
          response <- fmap responseBody $ runReq
            (defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing })
            (req GET url NoReqBody bsResponse $ port warpPort)
          response `shouldBe` "Something went wrong"
          item <- timeout 5_000_000 $ readMVar itemVar
          let portAsText = T.pack $ show warpPort
          (item >>= itemRequest) `shouldBe` Just
            ( Request
                { requestUrl = "http://localhost:" <> portAsText <> "/error"
                , requestMethod = "GET"
                , requestHeaders = KM.fromList
                    [ ("Accept-Encoding", "gzip")
                    , ("Host", String $ "localhost:" <> portAsText)
                    ]
                , requestParams = mempty
                , requestGet = mempty
                , requestQueryStrings = ""
                , requestPost = mempty
                , requestBody = ""
                , requestUserIp = ""
                }
            )


getSettingsAndItemVar :: IO (Settings, MVar Item)
getSettingsAndItemVar =
  (,) <$> pure testSettings
      <*> newEmptyMVar

-- | These specs never call the Rollbar API, so any token works.
testSettings :: Settings
testSettings = Settings
  { settingsToken = Token "invalid-token"
  , settingsEnvironment = Environment "test"
  , settingsRevision = Nothing
  , settingsRequestModifiers = defaultRequestModifiers
  }

withApp
  :: (MVar Item -> W.Port -> IO a)
  -> (Settings, MVar Item)
  -> IO a
withApp f (settings, itemVar) = do
  let waiSettings = W.setOnException
        (rollbarOnExceptionWith (createItemFake itemVar) settings)
        W.defaultSettings
  W.withApplicationSettings waiSettings (return app) $ f itemVar

app :: W.Application
app wrequest respond =
  case W.rawPathInfo wrequest of
    "/error" -> error "Boom"
    "/success" -> respond $ W.responseLBS status200 [] "OK"
    _ -> respond $ W.responseLBS status404 [] "Not Found"

-- | Hands the first reported 'Item' to the test instead of calling the API;
-- warp may report more than one exception per connection, so later ones are
-- ignored rather than overwriting it.
createItemFake :: MVar Item -> Item -> Rollbar ()
createItemFake itemVar item = do
  requestModifier <- getRequestModifier
  void $ liftIO $ tryPutMVar itemVar $
    item { itemRequest = requestModifier <$> itemRequest item }
