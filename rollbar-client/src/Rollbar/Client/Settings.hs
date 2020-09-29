module Rollbar.Client.Settings
  ( Settings(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data Settings = Settings
  { settingsToken :: ByteString
  , settingsEnvironment :: Text
  } deriving (Eq, Show)
