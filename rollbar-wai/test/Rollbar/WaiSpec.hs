{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.WaiSpec
  ( spec
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Rollbar.Client
import Rollbar.Wai
import System.Process
import Test.Hspec

spec :: Spec
spec =
  describe "rollbarOnExceptionWith" $
    it "sends information about the given request to Rollbar API" $ do
      settings <- readSettings "rollbar.yaml"
      itemRef <- newIORef Nothing
      let warpSettings = W.setOnException
            (rollbarOnExceptionWith (createItemFake itemRef) settings)
            W.defaultSettings
      port <- W.withApplicationSettings warpSettings (return app) $ \port -> do
        response <- readProcess "curl" ["-s", "http://localhost:" ++ show port] ""
        response `shouldBe` "Something went wrong"
        return $ T.pack $ show port

      mrequest <-fmap itemRequest <$> readIORef itemRef
      join mrequest `shouldBe` Just
        ( Request
          { requestUrl = "http://localhost:" <> port <> "/"
          , requestMethod = "GET"
          , requestHeaders = HM.fromList
              [ ("Accept", "*/*")
              , ("Host", String $ "localhost:" <> port)
              ]
          , requestParams = mempty
          , requestGet = mempty
          , requestQueryStrings = ""
          , requestPost = mempty
          , requestBody = ""
          , requestUserIp = ""
          }
        )


app :: W.Application
app _ _ = error "Boom"

createItemFake :: IORef (Maybe Item) -> Item -> Rollbar ()
createItemFake itemRef item = do
  requestModifier <- getRequestModifier
  liftIO $ writeIORef itemRef $ Just $
    item { itemRequest = requestModifier <$> itemRequest item }
