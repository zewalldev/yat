{-# LANGUAGE ScopedTypeVariables #-}

module CommandImpl (Command (..), exec) where

import Classes (HasTitle (..), ToString (..))
import ConfigOps (Config (..), initialize, isInitilaized, loadConfig)
import EditorOps (edit)
import HookOps (runPostHook, runPreHook)
import Ops (runOp)
import TaskClassesImpl ()
import TaskOps (addToRelease, createTodo, finishRelease, getFinishedTodo, getInprogressTodo, getRequestedTodo, listKeys, makeTodoCompleted, makeTodoInprogress, startRelease, taskFromString)
import Types (Command (..), Identifier (..), Inprogress (..), OperationCommand (..), Requested (..), Task (..), TaskStatus (..))
import UserOps (getUser)

exec :: Command -> IO ()
exec InitCommand = runOp $ do
  isInitilaized
  initialize
  pure "Yat has been initialized.\n"
exec (RequestTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir RequestOperation (TodoKey name)
  user <- getUser
  requested <- taskFromString =<< edit (toString Requested {_author = user, _task = Todo {_title = "", _description = ""}})
  createTodo rootDir name requested
  runPostHook rootDir RequestOperation (TodoKey name)
  pure $ "Todo " ++ name ++ " is requested\n"
exec (StartTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir StartOperation (TodoKey name)
  requested <- getRequestedTodo rootDir name
  user <- getUser
  inprogress <- taskFromString =<< edit (toString Inprogress {_implementer = user, _requested = requested})
  makeTodoInprogress rootDir name inprogress
  runPostHook rootDir StartOperation (TodoKey name)
  pure $ "Todo " ++ name ++ " is starting\n"
exec (FinishTaskCommand name) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir FinishOperation (TodoKey name)
  makeTodoCompleted rootDir name
  runPostHook rootDir FinishOperation (TodoKey name)
  pure $ "Todo " ++ name ++ " is finished.\n"
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
exec (StartReleaseCommand version) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir StartOperation (ReleaseKey version)
  startRelease rootDir version
  keys <- listKeys rootDir DoneStatus
  addToRelease rootDir version keys
  runPostHook rootDir StartOperation (ReleaseKey version)
  pure $ "Release " ++ version ++ " is started.\n"
exec (FinishReleaseCommand version) = runOp $ do
  Config rootDir <- loadConfig
  runPreHook rootDir FinishOperation (ReleaseKey version)
  finishRelease rootDir version
  runPostHook rootDir FinishOperation (ReleaseKey version)
  pure $ "Release " ++ version ++ " is finished.\n"