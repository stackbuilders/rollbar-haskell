{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Rollbar.YesodSpec
  ( spec
  ) where

import qualified Network.Wai as W

import Data.IORef
import Data.Maybe
import Rollbar.Client
import Rollbar.Yesod
import Test.Hspec
import Yesod.Core
import Yesod.Test

data App = App
  { appRollbarSettings :: Settings
  , appRequestRef :: IORef (Maybe W.Request)
  }

mkYesod "App" [parseRoutes|
  /error ErrorR GET
  /success SuccessR GET
|]

instance HasSettings Handler where
  getSettings = getsYesod appRollbarSettings

instance Yesod App where
  yesodMiddleware handler = do 
    requestRef <- getsYesod appRequestRef
    rollbarYesodMiddlewareWith (writeRequest requestRef) $
      defaultYesodMiddleware handler
    where
      writeRequest requestRef _ wrequest _ =
        liftIO $ writeIORef requestRef $ Just wrequest

getErrorR :: Handler ()
getErrorR = error "Boom"

getSuccessR :: Handler ()
getSuccessR = return ()

spec :: Spec
spec = withApp $
  describe "rollbarYesodMiddlewareWith" $ do
    context "when there is an error" $
      it "triggers a call to Rollbar" $ do
        get ErrorR
        statusIs 500
        requestRef <- appRequestRef <$> getTestYesod
        liftIO $ do
          wrequest <- readIORef requestRef
          wrequest `shouldSatisfy` isJust

    context "when there is no error" $
      it "does not trigger a call to Rollbar" $ do
        get SuccessR
        statusIs 200
        requestRef <- appRequestRef <$> getTestYesod
        liftIO $ do
          wrequest <- readIORef requestRef
          wrequest `shouldSatisfy` isNothing


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  app <- getApp
  return (app, id)
  where
    getApp =
      App <$> readSettings "rollbar.yaml"
          <*> newIORef Nothing
