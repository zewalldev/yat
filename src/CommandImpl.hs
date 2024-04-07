{-# LANGUAGE ScopedTypeVariables #-}

module CommandImpl (Command (..), exec) where

import Classes (HasTitle (..), ToString (..))
import ConfigOps (Config (..), initialize, isInitilaized, loadConfig)
import EditorOps (edit)
import HookOps (runPostHook, runPreHook)
import Ops (runOp)
import TaskClassesImpl ()
import TaskOps (createTodo, getFinishedTodo, getInprogressTodo, getRequestedTodo, listKeys, makeTodoCompleted, makeTodoInprogress, taskFromString)
import Types (Command (..), Inprogress (Inprogress, _implementer, _requested), OperationCommand (..), Requested (..), Task (..), TaskStatus (..))
import UserOps (getUser)

exec :: Command -> IO ()
exec InitCommand = runOp $ do
  isInitilaized
  initialize
  pure "Yat has been initialized."
exec (RequestTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir RequestOperation name
  user <- getUser
  text <- edit (toString Requested {_author = user, _task = Todo {_title = "", _description = ""}})
  requested <- taskFromString text
  createTodo rootDir name requested
  runPostHook rootDir RequestOperation name
  pure $ "Todo " ++ name ++ " is requested"
exec (StartTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir StartOperation name
  requested <- getRequestedTodo rootDir name
  user <- getUser
  text <- edit (toString Inprogress {_implementer = user, _requested = requested})
  inprogress <- taskFromString text
  makeTodoInprogress rootDir name inprogress
  runPostHook rootDir StartOperation name
  pure $ "Todo " ++ name ++ " is starting"
exec (FinishTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir FinishOperation name
  makeTodoCompleted rootDir name
  runPostHook rootDir FinishOperation name
  pure $ "Todo " ++ name ++ " is finished"
exec (ListTaskCommand status) = runOp $ do
  Config rootDir <- loadConfig
  keys <- listKeys rootDir status
  items <-
    mapM
      ( \key -> case status of
          RequestedStatus -> fmap (((key ++ ": ") ++) . title) (getRequestedTodo rootDir key)
          InprogressStatus -> fmap (((key ++ ": ") ++) . title) (getInprogressTodo rootDir key)
          DoneStatus -> fmap (((key ++ ": ") ++) . title) (getFinishedTodo rootDir key)
      )
      keys
  pure (unlines items)
