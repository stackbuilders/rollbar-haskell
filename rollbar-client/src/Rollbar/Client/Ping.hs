{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client.Ping
  ( -- ** Requests
    Pong(..)
    -- ** Endpoints
  , ping
  ) where

import Control.Monad (void)
import Network.HTTP.Req
import Rollbar.Client.Internal (baseUrl)

data Pong = Pong
  deriving (Eq, Show)

-- | Pings Rollbar API server, on success returns 'Pong'.
--
-- __Example__
--
-- > settings <- readSettings "rollbar.yaml"
-- > runRollbar settings ping
--
-- __Reference__
--
-- <https://explorer.docs.rollbar.com/#section/Ping>
ping :: MonadHttp m => m Pong
ping = do
  void $ req GET url NoReqBody ignoreResponse mempty
  return Pong
  where
    url = baseUrl /: "status" /: "ping"
