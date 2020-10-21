{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Rollbar.Yesod
-- Copyright: (c) 2020 Stack Builders Inc.
-- License: MIT
-- Maintainer: Sebastián Estrella <sestrella@stackbuilders.com>
--
-- For a fully working example check the following link:
--
-- <https://github.com/stackbuilders/rollbar-haskell/blob/master/rollbar-yesod/example/Main.hs>
module Rollbar.Yesod
  ( rollbarYesodMiddleware
  , rollbarYesodMiddlewareWith
  ) where

import qualified Network.Wai as W

import Control.Exception (Exception(..), SomeException)
import Control.Monad (unless)
import Rollbar.Client
import Rollbar.Wai (rollbarOnException)
import UnliftIO.Exception (catch, throwIO)
import Yesod.Core
import Yesod.Core.Types (HandlerContents)

-- | Captures non 'HandlerContents' exceptions and send them to Rollbar via the
-- API. Under the hood, this function uses 'createItem' function from
-- rollbar-client.
--
-- __Example__
--
-- > instance Yesod App where
-- >   yesodMiddleware = rollbarYesodMiddleware . defaultYesodMiddleware
rollbarYesodMiddleware
  :: (HasSettings m, MonadHandler m, MonadUnliftIO m)
  => m a
  -> m a
rollbarYesodMiddleware = rollbarYesodMiddlewareWith $ \settings request ex ->
  liftIO $ rollbarOnException settings (Just request) ex

-- | Similar to 'rollbarYesodMiddleware', but it allows customize the function
-- used to send the 'SomeException' captured from a handler to Rollbar.
rollbarYesodMiddlewareWith
  :: (HasSettings m, MonadHandler m, MonadUnliftIO m)
  => (Settings -> W.Request -> SomeException -> m ())
  -> m a
  -> m a
rollbarYesodMiddlewareWith f handler = handler `catch` \ex -> do
  unless (isHandlerContents ex) $ do
    settings <- getSettings
    wrequest <- waiRequest
    f settings wrequest ex

  throwIO ex

isHandlerContents :: SomeException -> Bool
isHandlerContents = maybe False handlerContents . fromException
  where
    handlerContents :: HandlerContents -> Bool
    handlerContents = const True
