module CommandImpl (Command (..), exec) where

import Core (doInit, newTodo)
import Entity (TaskName)

data Command
  = Init
  | NewTodo TaskName
  deriving (Show)

exec :: Command -> IO ()
exec Init = doInit
exec (NewTodo name) = newTodo name
