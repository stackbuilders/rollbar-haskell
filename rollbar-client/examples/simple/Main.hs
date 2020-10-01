{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS

import Rollbar.Client
import Rollbar.Client.Settings
import System.Environment

main :: IO ()
main = do
  settings <- getSettings
  withRollbar settings $ error "Boom"

getSettings :: IO Settings
getSettings =
  Settings <$> BS.pack <$> getEnv "ROLLBAR_TOKEN"
           <*> pure "test"
