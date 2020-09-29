module Rollbar.ClientSpec where

import Rollbar.Client
import Test.Hspec

spec :: Spec
spec = do
  describe "ping" $
    it "returns Pong" $
      run ping `shouldReturn` Pong
