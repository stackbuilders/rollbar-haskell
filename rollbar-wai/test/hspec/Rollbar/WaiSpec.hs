{-# LANGUAGE OverloadedStrings #-}

module Rollbar.WaiSpec
  ( spec
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Network.HTTP.Req
import Network.HTTP.Types (status200, status404)
import Rollbar.Client
import Rollbar.Wai (rollbarOnExceptionWith)
import Test.Hspec

spec :: Spec
spec = before getSettingsAndItemRef $
  describe "rollbarOnExceptionWith" $ do
    context "when the response status code is 200" $
      it "does not trigger a call to Rollbar" $
        withApp $ \itemRef warpPort -> do
          let url = http "localhost" /: "success"
          response <- runReq
            defaultHttpConfig
            (req GET url NoReqBody bsResponse $ port warpPort)
          responseStatusCode response `shouldBe` 200
          responseBody response `shouldBe` "OK"
          threadDelay 500
          readIORef itemRef `shouldReturn` Nothing

    context "when the response status code is not 200" $
      it "triggers a call to Rollbar" $
        withApp $ \itemRef warpPort -> do
          let url = http "localhost" /: "error"
          response <- responseBody <$> runReq
            (defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing })
            (req GET url NoReqBody bsResponse $ port warpPort)
          response `shouldBe` "Something went wrong"
          threadDelay 500
          let portAsText = T.pack $ show warpPort
          (itemRequest =<<) <$> readIORef itemRef `shouldReturn` Just
            ( Request
                { requestUrl = "http://localhost:" <> portAsText <> "/error"
                , requestMethod = "GET"
                , requestHeaders = HM.fromList
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


getSettingsAndItemRef :: IO (Settings, IORef (Maybe Item))
getSettingsAndItemRef =
  (,) <$> readSettings "rollbar.yaml"
      <*> newIORef Nothing

withApp
  :: (IORef (Maybe Item) -> W.Port -> IO a)
  -> (Settings, IORef (Maybe Item))
  -> IO a
withApp f (settings, itemRef) = do
  let waiSettings = W.setOnException
        (rollbarOnExceptionWith (createItemFake itemRef) settings)
        W.defaultSettings
  W.withApplicationSettings waiSettings (return app) $ f itemRef

app :: W.Application
app wrequest respond =
  case W.rawPathInfo wrequest of
    "/error" -> error "Boom"
    "/success" -> respond $ W.responseLBS status200 [] "OK"
    _ -> respond $ W.responseLBS status404 [] "Not Found"

createItemFake :: IORef (Maybe Item) -> Item -> Rollbar ()
createItemFake itemRef item = do
  requestModifier <- getRequestModifier
  liftIO $ writeIORef itemRef $ Just $
    item { itemRequest = requestModifier <$> itemRequest item }
