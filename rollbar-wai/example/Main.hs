module Main
  ( main
  ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Rollbar.Client (readSettings)
import Rollbar.Wai

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  let warpSettings = setOnException (rollbarOnException settings) defaultSettings
  runSettings warpSettings app

app :: Application
app _ _ = error "Boom"
