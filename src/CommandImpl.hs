module CommandImpl (Command (..), exec) where

import ConfigOps (Config(..), loadConfig)
import EditorOps (edit)
import HookOps (Entity (..), HookCommand (..), runPostHook, runPreHook)
import Ops (runOp)
import TodoOps (createTodo, getTodoContent, initialize, isInitilaized, makeTodoCompleted, makeTodoInprogress, setTodoContent, todoTemaplate, listTodo)
import Types (TaskFullName (..), TaskName, TaskStatus (..))

data Command
  = Init
  | RequestTodo TaskName
  | StartTodo TaskName
  | FinishTodo TaskName
  | ListTodo TaskStatus
  deriving (Show)

exec :: Command -> IO ()
exec Init = runOp $ do
  isInitilaized
  initialize
  pure "Yat has been initialized."
exec (RequestTodo name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir Request Todo name
  createTodo rootDir name todoTemaplate
  content <- edit todoTemaplate
  setTodoContent rootDir (MkTaskFullName Requested name) content
  runPostHook rootDir Request Todo name
  pure $ "Todo " ++ name ++ " is requested"
exec (StartTodo name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir Start Todo name
  content <- getTodoContent rootDir (MkTaskFullName Requested name)
  newContent <- edit content
  setTodoContent rootDir (MkTaskFullName Requested name) newContent
  makeTodoInprogress rootDir name
  runPostHook rootDir Start Todo name
  pure $ "Todo " ++ name ++ " is starting"
exec (FinishTodo name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir Finish Todo name
  makeTodoCompleted rootDir name
  runPostHook rootDir Finish Todo name
  pure $ "Todo " ++ name ++ " is finished"
exec (ListTodo status) = runOp $ do
  Config rootDir <- loadConfig
  listTodo rootDir status

