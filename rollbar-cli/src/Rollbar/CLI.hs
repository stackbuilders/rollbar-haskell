module Rollbar.CLI
  ( parseCommand
  , runCommand
  ) where

import Options.Applicative
import Rollbar.Client

data Command
  = CommandPing
  | CommandDeploy DeployCommand
  deriving (Eq, Show)

data DeployCommand = DeployCommandReport
  deriving (Eq, Show)

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
    , progDesc "Report a deploy"
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
