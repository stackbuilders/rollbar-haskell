{-# LANGUAGE FlexibleContexts #-}

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

rollbarYesodMiddleware
  :: (HasSettings m, MonadHandler m, MonadUnliftIO m)
  => m a
  -> m a
rollbarYesodMiddleware = rollbarYesodMiddlewareWith $ \settings request ex ->
  liftIO $ rollbarOnException settings (Just request) ex

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
