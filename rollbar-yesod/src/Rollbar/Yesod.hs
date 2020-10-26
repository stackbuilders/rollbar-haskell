{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad (unless, void)
import Rollbar.Client
import Rollbar.Wai (rollbarOnExceptionWith)
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
--
-- @since 0.1.0
rollbarYesodMiddleware
  :: (HasSettings m, MonadHandler m, MonadUnliftIO m)
  => m a
  -> m a
rollbarYesodMiddleware = rollbarYesodMiddlewareWith $ \settings request ex ->
  rollbarOnExceptionWith handler settings (Just request) ex
  where
    handler item = void $ createItem item { itemFramework = Just "yesod" }

-- | Similar to 'rollbarYesodMiddleware', but it allows customize the function
-- used to send the 'SomeException' captured from a handler to Rollbar.
--
-- @since 0.1.0
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
