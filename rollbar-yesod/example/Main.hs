{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main
  ( main
  ) where

import Network.Wai.Handler.Warp (run)
import Rollbar.Client
import Rollbar.Yesod (rollbarYesodMiddleware)
import Yesod.Core

newtype App = App { appRollbarSettings :: Settings }

mkYesod "App" [parseRoutes|
  / RootR GET
|]

instance HasSettings Handler where
  getSettings = getsYesod appRollbarSettings

instance Yesod App where
  yesodMiddleware = rollbarYesodMiddleware . defaultYesodMiddleware

getRootR :: Handler ()
getRootR = error "Boom"

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  toWaiApp (App settings) >>= run 3000
