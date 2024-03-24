{-# LANGUAGE LambdaCase #-}
module CommandParser (parseCommand) where

import CommandImpl (Command (..))
import Text.Parsec (char, digit, eof, letter, many, many1, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)
import Types (TaskStatus (..))

identifier :: Parser String
identifier = do
  h <- firstChar
  t <- many nonFirstChar
  return (h : t)
  where
    firstChar = letter
    nonFirstChar = digit <|> firstChar <|> char '-'

spaces1 :: Parser String
spaces1 = many1 space

parseInitCommand :: Parser Command
parseInitCommand = do
  _ <- string "init"
  _ <- spaces
  eof
  return Init

parseRequestTodoCommand :: Parser Command
parseRequestTodoCommand = do
  _ <- string "request"
  _ <- spaces1
  _ <- string "todo"
  _ <- spaces1
  name <- identifier
  eof
  return (RequestTodo name)

parseStartTodoCommand :: Parser Command
parseStartTodoCommand = do
  _ <- string "start"
  _ <- spaces1
  _ <- string "todo"
  _ <- spaces1
  name <- identifier
  eof
  return (StartTodo name)

parseFinishTodoCommand :: Parser Command
parseFinishTodoCommand = do
  _ <- string "finish"
  _ <- spaces1
  _ <- string "todo"
  _ <- spaces1
  name <- identifier
  eof
  return (FinishTodo name)

mapStatus :: String -> TaskStatus
mapStatus = \case
  "requested" -> Requested
  "inprogress" -> Inprogress
  "finished" -> Done
  _ -> Requested

parseTodoListCommand :: Parser Command
parseTodoListCommand = do
  _ <- string "list"
  _ <- spaces1
  status <- string "requested" <|> string "inprogress" <|> string "finished"
  _ <- spaces1
  _ <- string "todo"
  return (ListTodo $ mapStatus status)



parseCommand :: Parser Command
parseCommand =
  try parseInitCommand
    <|> try parseRequestTodoCommand
    <|> try parseStartTodoCommand
    <|> try parseFinishTodoCommand
    <|> try parseTodoListCommand
