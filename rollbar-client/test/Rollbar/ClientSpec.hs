{-# LANGUAGE OverloadedStrings #-}

module Rollbar.ClientSpec where

import qualified Data.ByteString.Char8 as BS

import Data.ByteString (ByteString)
import Network.HTTP.Req
import Rollbar.Client
import Rollbar.Client.Item
import Rollbar.Client.Settings (Settings(..))
import System.Environment
import Test.Hspec

spec :: Spec
spec = before getToken $ do
  describe "ping" $
    it "..." $ \_ ->
      run ping `shouldReturn` Pong

  describe "createItem" $ do
    context "PayloadTrace" $
      it "..." $ \token -> do
        let settings = Settings token "test"
        itemId <- runRollbar defaultHttpConfig settings $ do
          item <- mkItem $ PayloadTrace $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True

    context "PayloadTraceChain" $
      it "..." $ \token -> do
        let settings = Settings token "test"
        itemId <- runRollbar defaultHttpConfig settings $ do
          item <- mkItem $ PayloadTraceChain $ pure $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True


getToken :: IO ByteString
getToken = BS.pack <$> getEnv "ROLLBAR_TOKEN"


run :: Req a -> IO a
run = runReq defaultHttpConfig
