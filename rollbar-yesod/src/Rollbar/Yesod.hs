{-# LANGUAGE FlexibleContexts #-}

module Rollbar.Yesod
  ( rollbarYesodMiddleware
  ) where

import Control.Monad
import Control.Monad.Catch
import Rollbar.Client
import Rollbar.Wai (rollbarOnException)
import Yesod.Core
import Yesod.Core.Types (HandlerContents)

rollbarYesodMiddleware
  :: MonadCatch (HandlerFor site)
  => Settings
  -> HandlerFor site res
  -> HandlerFor site res
rollbarYesodMiddleware settings handler = handler `catch` \ex -> do
  unless (isHandlerContents ex) $ do
    req <- waiRequest
    rollbarOnException settings (Just req) ex

  throwM ex

isHandlerContents :: SomeException -> Bool
isHandlerContents = maybe False bar . fromException
  where
    bar :: HandlerContents -> Bool
    bar = const True
