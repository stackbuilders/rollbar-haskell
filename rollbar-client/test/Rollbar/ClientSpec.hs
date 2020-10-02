{-# LANGUAGE OverloadedStrings #-}

module Rollbar.ClientSpec where

import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Req
import Rollbar.Client
import System.Environment
import Test.Hspec

spec :: Spec
spec = before getSettings $ do
  describe "ping" $
    it "returns Pong" $ \_ ->
      run ping `shouldReturn` Pong

  describe "createItem" $ do
    context "PayloadTrace" $
      it "returns ItemId" $ \settings -> do
        itemId <- runRollbar defaultHttpConfig settings $ do
          item <- mkItem $ PayloadTrace $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True

    context "PayloadTraceChain" $
      it "returns ItemId" $ \settings -> do
        itemId <- runRollbar defaultHttpConfig settings $ do
          item <- mkItem $ PayloadTraceChain $ pure $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True


getSettings :: IO Settings
getSettings =
  Settings <$> BS.pack <$> getEnv "ROLLBAR_TOKEN"
           <*> pure "test"


run :: Req a -> IO a
run = runReq defaultHttpConfig
