{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS

import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings)
import Rollbar.Client.Settings (Settings(..))
import Rollbar.Wai (rollbarOnException)
import System.Environment

main :: IO ()
main = do
  settings <- getSettings
  let warpSettings = setOnException (rollbarOnException settings) defaultSettings
  runSettings warpSettings app

getSettings :: IO Settings
getSettings =
  Settings <$> BS.pack <$> getEnv "ROLLBAR_TOKEN"
           <*> pure "test"

app :: Application
app _ _ = error "Boom"
