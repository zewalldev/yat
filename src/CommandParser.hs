module CommandParser (parseCommand) where

import ParserUtils (identifier, spaces1)
import Text.Parsec
  ( anyChar,
    eof,
    manyTill,
    space,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Types (Command (..), TaskStatus (..))

parseInitCommand :: Parser Command
parseInitCommand = do
  _ <- string "init"
  _ <- spaces
  eof
  return InitCommand

parseRequestTodoCommand :: Parser Command
parseRequestTodoCommand = do
  _ <- string "request"
  _ <- spaces1
  name <- identifier
  eof
  return (RequestTaskCommand name)

parseStartTodoCommand :: Parser Command
parseStartTodoCommand = do
  _ <- string "start"
  _ <- spaces1
  name <- identifier
  eof
  return (StartTaskCommand name)

parseFinishTodoCommand :: Parser Command
parseFinishTodoCommand = do
  _ <- string "finish"
  _ <- spaces1
  name <- identifier
  eof
  return (FinishTaskCommand name)

parseRequestedStatus :: Parser TaskStatus
parseRequestedStatus = string "requested" >> pure RequestedStatus

parseInprogressStatus :: Parser TaskStatus
parseInprogressStatus = string "inprogress" >> pure InprogressStatus

parseFinishedStatus :: Parser TaskStatus
parseFinishedStatus = string "done" >> pure DoneStatus

parseTodoListCommand :: Parser Command
parseTodoListCommand = do
  _ <- string "list"
  _ <- spaces1
  status <- parseRequestedStatus <|> parseInprogressStatus <|> parseFinishedStatus
  eof
  return (ListTaskCommand status)

parseReleaseStartCommand :: Parser Command
parseReleaseStartCommand = do
  _ <- string "start"
  _ <- spaces1
  _ <- string "release"
  _ <- spaces1
  version <- manyTill anyChar (try eof)
  pure (StartReleaseCommand version)

parseReleaseFinishCommand :: Parser Command
parseReleaseFinishCommand = do
  _ <- string "finish"
  _ <- spaces1
  _ <- string "release"
  _ <- spaces1
  version <- manyTill anyChar (try eof)

  pure (FinishReleaseCommand version)

parseCommand :: Parser Command
parseCommand =
  try parseInitCommand
    <|> try parseRequestTodoCommand
    <|> try parseStartTodoCommand
    <|> try parseFinishTodoCommand
    <|> try parseTodoListCommand
    <|> try parseReleaseStartCommand
    <|> try parseReleaseFinishCommand
