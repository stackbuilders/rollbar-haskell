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

data DeployCommand = DeployCommandReport (Maybe Revision)
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
  command "report" $ info (deployCommandReportParser <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Tracks a deploy in Rollbar"
    ]

deployCommandReportParser :: Parser DeployCommand
deployCommandReportParser =
  DeployCommandReport <$> optional (Revision <$> strOption revisionOptions)
  where
    revisionOptions = mconcat
      [ short 'r'
      , long "revision"
      , help $ mconcat
        [ "Git SHA of revision being deployed, if this argument is not present"
        , " it would try to get the value from the configuration file first"
        , " before calling git."
        ]
      ]

runCommand :: Settings -> Command -> IO ()
runCommand settings cmd = do
  case cmd of
    CommandPing -> do
      pong <- runRollbar settings ping
      print pong
    CommandDeploy (DeployCommandReport mrevision) -> do
      deployId <- runRollbar settings $ do
        revision <- case mrevision of
          Nothing -> getRevision
          Just revision -> return revision
        deploy <- mkDeploy revision
        reportDeploy deploy

      print deployId
