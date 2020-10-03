module Rollbar.CLI where

import Options.Applicative

data Command = CommandDeploy DeployCommand
  deriving (Eq, Show)

data DeployCommand
  = DeployCommandReport
  | DeployCommandList
  deriving (Eq, Show)

commandP :: Parser Command
commandP = subparser $ mconcat
  [ command "deploy" $ info (CommandDeploy <$> deployCommandP) mempty
  ]

deployCommandP :: Parser DeployCommand
deployCommandP = subparser $ mconcat
  [ command "report" $ info (pure DeployCommandReport) mempty
  , command "list" $ info (pure DeployCommandList) mempty
  ]
