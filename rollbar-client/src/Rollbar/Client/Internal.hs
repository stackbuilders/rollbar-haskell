{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Rollbar.Client.Internal
  ( DataResponse(..)
  , ResultResponse(..)
  , rollbar
  , baseUrl
  ) where

import Data.Aeson
import Data.Proxy (Proxy)
import Network.HTTP.Req
import Rollbar.Client.Settings

newtype DataResponse a = DataResponse { unDataResponse :: a }
  deriving (Eq, Show)

instance FromJSON a => FromJSON (DataResponse a) where
  parseJSON = withObject "DataResponse" $ \o ->
    DataResponse <$> o .: "data"

data ResultResponse a = ResultResponse
  { resultResponseErr :: Integer
  , resultResponseResult :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (ResultResponse a) where
  parseJSON = withObject "ResultResponse" $ \o ->
    ResultResponse <$> o .: "err"
                   <*> o .: "result"

rollbar
  :: ( HasSettings m
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     , HttpMethod method
     , HttpResponse response
     , MonadHttp m
     )
  => method
  -> Url 'Https
  -> body
  -> Proxy response
  -> m response
rollbar method url body response = do
  Token token <- settingsToken <$> getSettings
  req method url body response $ header "X-Rollbar-Access-Token" token

baseUrl :: Url 'Https
baseUrl = https "api.rollbar.com" /: "api" /: "1"
