module Main
  ( main
  ) where

import Rollbar.CLI (parseCommand, runCommand)
import Rollbar.Client (readSettings)

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  parseCommand >>= runCommand settings
