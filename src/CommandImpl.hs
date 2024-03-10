module CommandImpl (Command (..), exec) where

import Core (doInit)

data Command
  = Init
  deriving (Show)

exec :: Command -> IO ()
exec Init = doInit