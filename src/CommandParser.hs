module CommandParser (parseCommand) where

import CommandImpl (Command (..))
import Text.Parsec (char, digit, eof, letter, many, many1, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

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

parseNewTodoCommand :: Parser Command
parseNewTodoCommand = do
  _ <- string "new"
  _ <- spaces1
  _ <- string "todo"
  _ <- spaces1
  name <- identifier
  eof
  return (NewTodo name)

parseCommand :: Parser Command
parseCommand =
  try parseInitCommand
    <|> try parseNewTodoCommand
