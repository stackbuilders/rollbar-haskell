module Main
  ( main
  ) where

import Rollbar.Client

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  withRollbar settings $ error "Boom"
