-- |
-- Module: Rollbar.CLI
-- Copyright: (c) 2020 Stack Builders Inc.
-- License: MIT
-- Maintainer: Sebastián Estrella <sestrella@stackbuilders.com>
module Rollbar.CLI
  ( Command(..)
  , DeployCommand(..)
  , parseCommand
  , runCommand
  ) where

import Options.Applicative
import Rollbar.Client

data Command
  = CommandPing
    -- ^ Pings Rollbar API server.
    --
    -- @since 0.1.0
  | CommandDeploy DeployCommand
    -- ^ Tracks a deploy in Rollbar.
    --
    -- @since 0.1.0
  deriving (Eq, Show)

data DeployCommand = DeployCommandReport
  deriving (Eq, Show)

-- | Parses a 'Command'.
--
-- @since 0.1.0
parseCommand :: IO Command
parseCommand = execParser commandParserInfo

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Simple CLI to talk with Rollbar API"
  ]

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "ping" $ info (pingParser <**> helper) $ mconcat
      [ fullDesc
      , progDesc "Ping the API server"
      ]
  , command "deploy" $ info (deployParser <**> helper) $ mconcat
      [ fullDesc
      , progDesc "Deploy specific commands"
      ]
  ]
  where
    pingParser = pure CommandPing
    deployParser = CommandDeploy <$> deployCommandParser

deployCommandParser :: Parser DeployCommand
deployCommandParser = subparser $
  command "report" $ info (pure DeployCommandReport) $ mconcat
    [ fullDesc
    , progDesc "Tracks a deploy in Rollbar"
    ]

runCommand :: Settings -> Command -> IO ()
runCommand settings cmd = do
  case cmd of
    CommandPing -> do
      pong <- runRollbar settings ping
      print pong
    CommandDeploy DeployCommandReport -> do
      deployId <- runRollbar settings $ do
        deploy <- getRevision >>= mkDeploy
        reportDeploy deploy

      print deployId
