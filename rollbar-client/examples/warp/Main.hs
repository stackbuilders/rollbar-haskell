{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS

import Control.Exception
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings)
import Rollbar.Client.Settings
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

rollbarOnException :: Settings -> Maybe Request -> SomeException -> IO ()
rollbarOnException = undefined

app :: Application
app = error "Boom"
