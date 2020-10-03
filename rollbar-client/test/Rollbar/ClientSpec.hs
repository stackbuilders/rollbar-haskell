{-# LANGUAGE OverloadedStrings #-}

module Rollbar.ClientSpec
  ( spec
  ) where

import Rollbar.Client
import Test.Hspec

spec :: Spec
spec = before (readSettings "rollbar.yaml") $ do
  describe "ping" $
    it "returns Pong" $ \settings ->
      runRollbar settings ping `shouldReturn` Pong

  describe "createItem" $ do
    context "PayloadTrace" $
      it "returns ItemId" $ \settings -> do
        itemId <- runRollbar settings $ do
          item <- mkItem $ PayloadTrace $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True

    context "PayloadTraceChain" $
      it "returns ItemId" $ \settings -> do
        itemId <- runRollbar settings $ do
          item <- mkItem $ PayloadTraceChain $ pure $ Trace [] (mkException "NameError")
            { exceptionMessage = Just "global name 'foo' is not defined"
            , exceptionDescription = Just "Something went wrong while trying to save the user object"
            }
          createItem item

        itemId `shouldSatisfy` const True

  describe "reportDeploy" $
    it "returns DeployId" $ \settings -> do
      deployId <- runRollbar settings $ do
        deploy <- mkDeploy
        reportDeploy deploy

      deployId `shouldSatisfy` (> 0)
