{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rollbar.Client
  ( module Rollbar.Client.Deploy
  , module Rollbar.Client.Item
  , module Rollbar.Client.Ping
  , module Rollbar.Client.Settings
  , Rollbar(..)
  , withRollbar
  , runRollbar
  ) where

import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Trans (MonadTrans(..))
import Network.HTTP.Req
import Rollbar.Client.Deploy
import Rollbar.Client.Item
import Rollbar.Client.Ping
import Rollbar.Client.Settings

newtype Rollbar a = Rollbar (ReaderT Settings Req a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    )

instance MonadHttp Rollbar where
  handleHttpException = Rollbar . lift . handleHttpException

instance HasSettings Rollbar where
  getSettings = Rollbar ask

-- | Runs a computation, captures any 'E.SomeException' threw, and send it to
-- Rollbar.
withRollbar :: (MonadCatch m, MonadIO m) => Settings -> m a -> m a
withRollbar settings f = f `catch` \e -> do
  void $ runRollbar settings $ do
    item <- mkItem $ PayloadTrace $ Trace [] $ mkException (e :: SomeException)
    createItem item

  throwM e

-- | Run a computation in 'Rollbar' monad.
runRollbar :: MonadIO m => Settings -> Rollbar a -> m a
runRollbar settings (Rollbar f) = runReq defaultHttpConfig $
  runReaderT f settings
