module CommandImpl (Command (..), exec) where

import Core (doInit, finishTodo, newTodo, startTodo)
import Entity (TaskName)

data Command
  = Init
  | NewTodo TaskName
  | StartTodo TaskName
  | FinishTodo TaskName
  deriving (Show)

exec :: Command -> IO ()
exec Init = doInit
exec (NewTodo name) = newTodo name
exec (StartTodo name) = startTodo name
exec (FinishTodo name) = finishTodo name
