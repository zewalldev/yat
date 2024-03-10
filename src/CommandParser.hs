module CommandParser (parseCommand) where

import CommandImpl (Command (..))
import Text.Parsec (eof, spaces, string, try)
import Text.Parsec.String (Parser)

parseInitCommand :: Parser Command
parseInitCommand = do
  _ <- string "init"
  _ <- spaces
  eof
  return Init

parseCommand :: Parser Command
parseCommand = try parseInitCommand