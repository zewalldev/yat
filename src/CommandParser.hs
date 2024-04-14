module CommandParser (parseArgs) where

import Control.Applicative ((<|>))
import Options.Applicative (Parser, ParserInfo, argument, command, commandGroup, execParser, fullDesc, header, help, helper, info, metavar, progDesc, str, subparser)
import Types (Command (..), ReleaseVersion, TaskKey, TaskStatus (DoneStatus, InprogressStatus, RequestedStatus))

parseArgs :: IO Command
parseArgs = execParser commands

commands :: ParserInfo Command
commands =
  info
    (helper <*> pCommands)
    ( fullDesc
        <> header "yat - Yet another tracker"
        <> progDesc "Simple console file base task tracker"
    )

pCommands :: Parser Command
pCommands = pInitCommands <|> pTaskFlowCommands <|> pReleaseFlowCommands <|> pReportCommands

pTaskFlowCommands :: Parser Command
pTaskFlowCommands =
  subparser
    ( command "request" (info (helper <*> (RequestTaskCommand <$> pTaskKey)) (progDesc "Reuqest Todo"))
        <> command "start" (info (helper <*> (StartTaskCommand <$> pTaskKey)) (progDesc "Start Todo"))
        <> command "finish" (info (helper <*> (FinishTaskCommand <$> pTaskKey)) (progDesc "Finish Todo"))
        <> commandGroup "Task flow commands"
        <> metavar "TASK_FLOW_COMMANDS"
    )

pReleaseFlowCommands :: Parser Command
pReleaseFlowCommands =
  subparser
    ( command
        "release"
        ( info
            ( helper
                <*> subparser
                  ( command "start" (info (helper <*> (StartReleaseCommand <$> pReleaseVersion)) (progDesc "Start release"))
                      <> command "finish" (info (helper <*> (FinishReleaseCommand <$> pReleaseVersion)) (progDesc "Finish release"))
                  )
            )
            (progDesc "Release commands")
        )
        <> commandGroup "Release flow commands"
        <> metavar "RELEASE_FLOW_COMMANDS"
    )

pInitCommands :: Parser Command
pInitCommands =
  subparser
    ( command "init" (info (helper <*> pure InitCommand) (progDesc "Initialize yat"))
        <> commandGroup "Init commands"
        <> metavar "INIT_COMMANDS"
    )

pReportCommands :: Parser Command
pReportCommands =
  subparser
    ( command
        "list"
        ( info
            ( helper
                <*> ( ListTaskCommand
                        <$> subparser
                          ( command "requested" (info (helper <*> pure RequestedStatus) (progDesc "List requested"))
                              <> command "inprogress" (info (helper <*> pure InprogressStatus) (progDesc "List inprogress"))
                              <> command "done" (info (helper <*> pure DoneStatus) (progDesc "List finished"))
                          )
                    )
            )
            (progDesc "List commands")
        )
        <> commandGroup "Report commands"
        <> metavar "REPROT_COMMANDS"
    )

pTaskKey :: Parser TaskKey
pTaskKey = argument str (metavar "KEY" <> help "Task Key")

pReleaseVersion :: Parser ReleaseVersion
pReleaseVersion = argument str (metavar "VERSION" <> help "Release Version")
