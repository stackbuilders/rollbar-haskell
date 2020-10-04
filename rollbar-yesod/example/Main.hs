{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import Network.Wai.Handler.Warp (run)
import Rollbar.Client
import Rollbar.Yesod
import Yesod.Core

newtype App = App { appRollbarSettings :: Settings }

mkYesod "App" [parseRoutes|
  / RootR GET
|]

instance Yesod App where
  yesodMiddleware handler = do
    settings <- getsYesod appRollbarSettings
    rollbarYesodMiddleware settings $ defaultYesodMiddleware handler

getRootR :: Handler ()
getRootR = error "Boom"

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  toWaiApp (App settings) >>= run 3000
