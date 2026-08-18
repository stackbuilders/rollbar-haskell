{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rollbar.ClientSpec
  ( spec
  ) where

import qualified Control.Exception as E
import qualified Data.Aeson.KeyMap as KM

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as DBL
import Data.Text.Encoding
import Data.Text as T
import Data.Yaml.Config
import Rollbar.Client
import Test.Hspec

data Package = Package
  { packageName :: Text
  , packageVersion :: Text
  } deriving (Eq, Show)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \o ->
    Package <$> o .: "name"
            <*> o .: "version"

instance HasSettings (Reader Settings) where
  getSettings = ask

-- | An exception rendered across several lines, the way real ones are.
newtype TestException = TestException String
  deriving (Eq, Show)

instance E.Exception TestException where
  displayException (TestException detail) =
    "Something went wrong: " <> detail <> "\n" <>
    "CallStack (from HasCallStack):\n" <>
    "  error, called at src/Main.hs:42:9 in main:Main"

-- | An exception reported through the async exception hierarchy.
data AsyncTestException = AsyncTestException
  deriving Show

instance E.Exception AsyncTestException where
  toException = E.asyncExceptionToException
  fromException = E.asyncExceptionFromException

-- | An exception rendered verbatim, to exercise first-line edge cases.
newtype RawException = RawException String
  deriving Show

instance E.Exception RawException where
  displayException (RawException s) = s

spec :: Spec
spec = do
  describe "getRequestModifier" $ do
    let mkSettings modifiers = Settings
          { settingsToken = Token "invalid-token"
          , settingsEnvironment = Environment "test"
          , settingsRevision = Nothing
          , settingsRequestModifiers = modifiers
          }
        request = Request
          { requestUrl = "http://example.com"
          , requestMethod = "GET"
          , requestHeaders = KM.fromList
              [ ("Host", "example.com")
              , ("Secret", "p4ssw0rd")
              ]
          , requestParams = KM.fromList
              [ ("user", "John Doe")
              , ("password", "p4ssw0rd")
              ]
          , requestGet = mempty
          , requestQueryStrings = ""
          , requestPost = mempty
          , requestBody = ""
          , requestUserIp = ""
          }

    it "excludes the headers not matching the given names" $
      let requestModifier = runReader getRequestModifier $ mkSettings $
            defaultRequestModifiers
              { requestModifiersExcludeHeaders = Just $ pure "Secret" }
      in requestModifier request `shouldBe` request
           { requestHeaders = KM.fromList [("Host", "example.com")] }

    it "excludes the params not matching the given names" $
      let requestModifier = runReader getRequestModifier $ mkSettings $
            defaultRequestModifiers
              { requestModifiersExcludeParams = Just $ pure "password" }
      in requestModifier request `shouldBe` request
           { requestParams = KM.fromList [("user", "John Doe")] }

    it "includes only the headers matching the given names" $
      let requestModifier = runReader getRequestModifier $ mkSettings $
            defaultRequestModifiers
              { requestModifiersIncludeHeaders = Just $ pure "Host" }
      in requestModifier request `shouldBe` request
           { requestHeaders = KM.fromList [("Host", "example.com")] }

    it "includes only the params matching the given names" $
      let requestModifier = runReader getRequestModifier $ mkSettings $
            defaultRequestModifiers
              { requestModifiersIncludeParams = Just $ pure "user" }
      in requestModifier request `shouldBe` request
           { requestParams = KM.fromList [("user", "John Doe")] }

  describe "defaultNotifier" $
    it "matches the package name and version" $ do
      Package{..} <- loadYamlSettings ["package.yaml"] [] ignoreEnv
      defaultNotifier `shouldBe` Notifier
        { notifierName = packageName
        , notifierVersion = packageVersion
        }

  describe "mkException" $ do
    let rendered = T.pack $ E.displayException $ TestException "42"

    it "uses the exception type name as the class" $
      exceptionClass (mkException $ TestException "42") `shouldBe` "TestException"

    it "unwraps SomeException to reach the concrete exception type" $
      exceptionClass (mkException $ E.toException $ TestException "42")
        `shouldBe` "TestException"

    it "unwraps SomeAsyncException to reach the concrete exception type" $
      exceptionClass (mkException AsyncTestException)
        `shouldBe` "AsyncTestException"

    it "gives occurrences of the same type the same class" $
      exceptionClass (mkException $ TestException "42")
        `shouldBe` exceptionClass (mkException $ TestException "1337")

    it "puts the first line of the rendered exception in the message" $
      exceptionMessage (mkException $ TestException "42")
        `shouldBe` Just "Something went wrong: 42"

    it "keeps the whole rendered exception in the description" $
      exceptionDescription (mkException $ TestException "42")
        `shouldBe` Just rendered

    it "drops a trailing carriage return from the message" $
      exceptionMessage (mkException $ RawException "Boom\r\nDetails")
        `shouldBe` Just "Boom"

    it "omits the message when the first rendered line is blank" $
      exceptionMessage (mkException $ RawException "\nDetails")
        `shouldBe` Nothing

  before (readSettings "rollbar.yaml") $ do
    describe "ping" $
      it "returns Pong" $ \settings ->
        runRollbar settings ping `shouldReturn` Pong

    describe "createItem" $ do
      context "PayloadTrace" $
        it "returns ItemId" $ \settings -> do
          itemId <- runRollbar settings $ do
            item <- mkItem $ PayloadTrace $ Trace [] $ Exception
              { exceptionClass = "NameError"
              , exceptionMessage = Just "global name 'foo' is not defined"
              , exceptionDescription = Just "Something went wrong while trying to save the user object"
              }
            createItem item

          itemId `shouldSatisfy` const True

      context "PayloadTraceChain" $
        it "returns ItemId" $ \settings -> do
          itemId <- runRollbar settings $ do
            item <- mkItem $ PayloadTraceChain $ pure $ Trace [] $ Exception
              { exceptionClass = "NameError"
              , exceptionMessage = Just "global name 'foo' is not defined"
              , exceptionDescription = Just "Something went wrong while trying to save the user object"
              }
            createItem item

          itemId `shouldSatisfy` const True

      context "PayloadMessage" $
        it "returns ItemId" $ \settings -> do
          itemId <- runRollbar settings $ do
            item <- mkItem $ PayloadMessage $ Message
              { messageBody = "Request over threshold of 10 seconds"
              , messageMetadata = KM.fromList
                  [ ("route", "home#index")
                  , ("time_elapsed", Number 15.23)
                  ]
              }
            createItem item

          itemId `shouldSatisfy` const True

    describe "reportDeploy" $
      it "returns DeployId" $ \settings -> do
        deployId <- runRollbar settings $ do
          deploy <- getRevision >>= mkDeploy
          reportDeploy deploy

        deployId `shouldSatisfy` (> 0)

  describe "ToJSON Item" $ do
    context "when serializing to JSON" $ do
      let item = Item
            { itemEnvironment = Environment "test"
            , itemBody = Body { bodyPayload = PayloadMessage (Message "Test" mempty) }
            , itemLevel = Just LevelInfo
            , itemPlatform = Just "haskell"
            , itemLanguage = Just "Haskell"
            , itemFramework = Just "GHC"
            , itemRequest = Nothing
            , itemServer = Nothing
            , custom = Just mempty
            , title = Nothing
            , uuid = Just "12345"
            , fingerprint = Nothing
            , itemNotifier = Notifier "rollbar-client" "1.2.0"
            }
          jsonItem = decodeUtf8 $ DBL.toStrict $ encode item

      it "omits fields if they are Nothing values" $
        T.unpack jsonItem `shouldNotContain` "\"server\""

      it "includes fields if they are Just values" $
        T.unpack jsonItem `shouldContain` "\"platform\":\"haskell\""
