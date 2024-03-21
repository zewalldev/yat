module CommandImpl (Command (..), exec) where

import Core (doInit, finishTodo, requestTodo, startTodo)
import Types (TaskName)

data Command
  = Init
  | RequestTodo TaskName
  | StartTodo TaskName
  | FinishTodo TaskName
  deriving (Show)

exec :: Command -> IO ()
exec Init = doInit
exec (RequestTodo name) = requestTodo name
exec (StartTodo name) = startTodo name
exec (FinishTodo name) = finishTodo name
