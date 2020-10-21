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
  runSettings
    (setOnException (rollbarOnException settings) defaultSettings)
    app

app :: Application
app _ _ = error "Boom"
