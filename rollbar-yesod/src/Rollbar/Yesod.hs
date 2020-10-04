{-# LANGUAGE FlexibleContexts #-}

module Rollbar.Yesod
  ( rollbarYesodMiddleware
  ) where

import Control.Exception (Exception(..), SomeException)
import Control.Monad (unless, void)
import Rollbar.Client
import Rollbar.Wai (rollbarOnException)
import UnliftIO.Exception (catch, throwIO)
import Yesod.Core
import Yesod.Core.Types (HandlerContents)

rollbarYesodMiddleware
  :: (MonadHandler m, MonadUnliftIO m)
  => Settings
  -> m a
  -> m a
rollbarYesodMiddleware settings handler = handler `catch` \ex -> do
  unless (isHandlerContents ex) $ do
    req <- waiRequest
    rollbarOnException settings (Just req) ex

  throwIO ex

isHandlerContents :: SomeException -> Bool
isHandlerContents = maybe False handlerContents . fromException
  where
    handlerContents :: HandlerContents -> Bool
    handlerContents = const True
