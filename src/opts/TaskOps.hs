{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TaskOps
  ( createTodo,
    makeTodoInprogress,
    makeTodoCompleted,
    taskFromString,
    listKeys,
    getRequestedTodo,
    getInprogressTodo,
    getFinishedTodo,
  )
where

import ConstPath (donePath, inprogressPath, requestedPath)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Ops (Op, OpError (..))
import System.Directory (doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath ((<.>), (</>))
import Types (Inprogress (..), Requested (..), TaskStatus (..), Done, TaskKey)
import Classes (FromString (fromString), FromStringError (..), ToString (..))
import TaskClassesImpl()

getTask :: (FromString a) => FilePath -> Op a
getTask path = do
  fileExists <- liftIO $ doesFileExist path
  if fileExists
    then do
      content <- liftIO $ readFile path
      taskFromString content
    else throwE $ OpError "Todo not found"

makeTodoCompleted :: FilePath -> TaskKey -> Op ()
makeTodoCompleted root name = do
  let from = root </> inprogressPath </> name <.> "todo"
  let to = root </> donePath </> name <.> "todo"
  isRequested <- liftIO $ doesFileExist from
  todoIsInprogress <- liftIO $ doesFileExist to
  if isRequested
    then
      if not todoIsInprogress
        then do
          liftIO $ renameFile from to
          pure ()
        else throwE $ OpError "Todo is already in progress."
    else throwE $ OpError "Todo doesn't exists"

makeTodoInprogress :: FilePath -> TaskKey -> Inprogress -> Op ()
makeTodoInprogress root name inprogress = do
  let from = root </> requestedPath </> name <.> "todo"
  let to = root </> inprogressPath </> name <.> "todo"
  isRequested <- liftIO $ doesFileExist from
  todoIsInprogress <- liftIO $ doesFileExist to
  if isRequested
    then
      if not todoIsInprogress
        then do
          liftIO $ removeFile from
          liftIO . writeFile to $ toString inprogress
          pure ()
        else throwE $ OpError "Todo is already in progress."
    else throwE $ OpError "Todo doesn't exists"

createTodo :: FilePath -> TaskKey -> Requested -> Op ()
createTodo root name content = do
  let fn = root </> requestedPath </> name <.> "todo"
  isAlreadyExists <- liftIO $ doesFileExist fn
  if isAlreadyExists
    then throwE $ OpError "Todo already exists"
    else do
      liftIO . writeFile fn $ toString content
      pure ()

listKeys :: FilePath -> TaskStatus -> Op [String]
listKeys root status = do
  files <- liftIO . listDirectory $ root </> path
  let keys = fmap (takeWhile ('.' /=)) files
  pure keys
  where
    path = case status of
      RequestedStatus -> requestedPath
      InprogressStatus -> inprogressPath
      DoneStatus -> donePath

getRequestedTodo :: FilePath -> String -> Op Requested
getRequestedTodo root key = getTask (root </> requestedPath </> key <.> "todo")

getInprogressTodo :: FilePath -> String -> Op Inprogress
getInprogressTodo root key = getTask (root </> inprogressPath </> key <.> "todo")

getFinishedTodo :: FilePath -> String -> Op Done
getFinishedTodo root key = getTask (root </> donePath </> key <.> "todo")

taskFromString :: (FromString a) => String -> Op a
taskFromString content = do
  let todo = fromString content
  case todo of
    Left (FromStringError {_msg = msg}) -> throwE (OpError msg)
    Right task -> pure task
