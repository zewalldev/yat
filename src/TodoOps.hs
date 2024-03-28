{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module TodoOps (isInitilaized, initialize, todoTemaplate, createTodo, setTodoContent, makeTodoInprogress, getTodoContent, makeTodoCompleted, listTodo) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (catMaybes)
import Ops (Op, OpError (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, renameFile)
import System.FilePath ((<.>), (</>))
import TaskParser (parseTodo)
import Types (TaskFullName (..), TaskName, TaskStatus (Done, Inprogress, Requested), TodoContent (..))

yatDir, trackDir, requestedDir, inprogressDir, doneDir, releaseDir, releasedDir, confDir, hooksDir :: FilePath
yatDir = ".yat"
trackDir = "track"
requestedDir = "requested"
inprogressDir = "inprogress"
doneDir = "done"
releaseDir = "release"
releasedDir = "released"
confDir = "conf"
hooksDir = "hooks"

requestedPath, inprogressPath, donePath, releasePath, releasedPath, hooksPath :: FilePath
requestedPath = yatDir </> trackDir </> requestedDir
inprogressPath = yatDir </> trackDir </> inprogressDir
donePath = yatDir </> trackDir </> doneDir
releasePath = yatDir </> trackDir </> releaseDir
releasedPath = yatDir </> trackDir </> releasedDir
hooksPath = yatDir </> confDir </> hooksDir

isInitilaized :: Op ()
isInitilaized = do
  exists <- liftIO $ doesDirectoryExist yatDir
  if exists then pure () else throwE $ OpError "Yat already initialized."

initialize :: Op ()
initialize = do
  liftIO $ createDirectoryIfMissing True requestedPath
  liftIO $ createDirectoryIfMissing True inprogressPath
  liftIO $ createDirectoryIfMissing True donePath
  liftIO $ createDirectoryIfMissing True releasePath
  liftIO $ createDirectoryIfMissing True releasedPath
  liftIO $ createDirectoryIfMissing True hooksPath

makeTodoCompleted :: FilePath -> TaskName -> Op ()
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

makeTodoInprogress :: FilePath -> TaskName -> Op ()
makeTodoInprogress root name = do
  let from = root </> requestedPath </> name <.> "todo"
  let to = root </> inprogressPath </> name <.> "todo"
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

createTodo :: FilePath -> TaskName -> String -> Op ()
createTodo root name content = do
  let fn = root </> requestedPath </> name <.> "todo"
  isAlreadyExists <- liftIO $ doesFileExist fn
  if isAlreadyExists
    then throwE $ OpError "Todo already exists"
    else do
      liftIO . writeFile fn $ content
      pure ()

setTodoContent :: FilePath -> TaskFullName -> String -> Op ()
setTodoContent root (MkTaskFullName status name) content = do
  let fn = root </> taskStatusToPath status </> name <.> "todo"
  fileExists <- liftIO . doesFileExist $ fn
  if fileExists
    then do
      liftIO . writeFile fn $ content
      pure ()
    else throwE $ OpError "Todo not found"

getTodoContent :: FilePath -> TaskFullName -> Op String
getTodoContent root (MkTaskFullName status name) = do
  let fn = root </> taskStatusToPath status </> name <.> "todo"
  fileExists <- liftIO $ doesFileExist fn
  if fileExists
    then do
      liftIO $ readFile fn
    else throwE $ OpError "Todo not found"

taskStatusToPath :: TaskStatus -> FilePath
taskStatusToPath status = case status of
  Requested -> requestedPath
  Inprogress -> inprogressPath
  Done -> donePath

todoTemaplate :: String
todoTemaplate = unlines ["@title: ", "@author: ", "@implementor: ", "@description", "", "@end"]

todoPath :: TaskStatus -> FilePath
todoPath Requested = requestedPath
todoPath Inprogress = inprogressPath
todoPath Done = donePath

newtype TodoList = TodoList [(TaskName, TodoContent)]

instance Show TodoList where
  show (TodoList items) = unlines $ map (\it -> fst it ++ ": " ++ taskTitle (snd it)) items

todoName :: String -> String
todoName = takeWhile ('.' /=)

listTodo :: FilePath -> TaskStatus -> Op TodoList
listTodo root status = do
  files <- liftIO . listDirectory $ root </> todoPath status
  mbTodos <- mapM (\f -> (fmap . fmap) (todoName f,) (liftIO . parseTodo $ (root </> todoPath status </> f))) files
  let todos = catMaybes mbTodos
  pure (TodoList todos)