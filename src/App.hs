module App (app) where

import CommandImpl (exec)
import CommandParser (parseCommand)
import System.Environment (getArgs)
import Text.Parsec (runParser)

app :: IO ()
app = getArgs >>= either print exec . runParser parseCommand () "" . unwords
