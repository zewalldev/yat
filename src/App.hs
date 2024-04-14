module App (app) where

import CommandImpl (exec)
import CommandParser (parseArgs)

app :: IO ()
app = parseArgs >>= exec
