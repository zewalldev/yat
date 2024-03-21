module TodoOperation (isInitilaized, initialize, makeTodoCompleted, getTodoContent, setTodoContent, makeTodoInprogress, todoTemaplate, createTodo) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, renameFile)
import System.FilePath ((<.>), (</>))
import Types (TaskFullName (..), TaskName, TaskStatus (Done, Inprogress, Requested))

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

isInitilaized :: IO Bool
isInitilaized = doesDirectoryExist yatDir

initialize :: IO ()
initialize = do
  createDirectoryIfMissing True requestedPath
  createDirectoryIfMissing True inprogressPath
  createDirectoryIfMissing True donePath
  createDirectoryIfMissing True releasePath
  createDirectoryIfMissing True releasedPath
  createDirectoryIfMissing True hooksPath

-- data TodoStage = Requested | Inprogress | Done

-- doesTodoExist :: TodoStage -> TaskName -> IO Bool
-- doesTodoExist Requested name = doesFileExist (requestedPath </> name <.> "todo")
-- doesTodoExist Inprogress name = doesFileExist (inprogressPath </> name <.> "todo")
-- doesTodoExist Done name = doesFileExist (donePath </> name <.> "todo")

makeTodoCompleted :: FilePath -> TaskName -> IO (Either String ())
makeTodoCompleted root name = do
  let from = root </> inprogressPath </> name <.> "todo"
  let to = root </> donePath </> name <.> "todo"
  todoInProgress <- doesFileExist from
  todoIsDone <- doesFileExist to
  if todoInProgress
    then
      if not todoIsDone
        then do
          renameFile from to
          pure (Right ())
        else pure (Left "Todo is already done.")
    else pure (Left "Todo doesn't exists")

makeTodoInprogress :: FilePath -> TaskName -> IO (Either String ())
makeTodoInprogress root name = do
  let from = root </> requestedPath </> name <.> "todo"
  let to = root </> inprogressPath </> name <.> "todo"
  isRequested <- doesFileExist from
  todoIsInprogress <- doesFileExist to
  if isRequested
    then
      if not todoIsInprogress
        then do
          renameFile from to
          pure (Right ())
        else pure (Left "Todo is already in progress.")
    else pure (Left "Todo doesn't exists")

createTodo :: FilePath -> TaskName -> String -> IO (Either String ())
createTodo root name content = do
  let fn = root </> requestedPath </> name <.> "todo"
  isAlreadyExists <- doesFileExist fn
  if isAlreadyExists
    then pure (Left "Todo already exists")
    else do
      writeFile fn content
      pure (Right ())


getTodoContent :: FilePath -> TaskFullName -> IO (Maybe String)
getTodoContent root (MkTaskFullName status name) = do
  let fn = root </> taskStatusToPath status </> name <.> "todo"
  fileExists <- doesFileExist fn
  if fileExists
    then do
      content <- readFile fn
      pure (Just content)
    else pure Nothing

setTodoContent :: FilePath -> TaskFullName -> String -> IO (Either String ())
setTodoContent root (MkTaskFullName status name) content = do
  let fn = root </> taskStatusToPath status </> name <.> "todo"
  fileExists <- doesFileExist fn
  if fileExists
    then do
      writeFile fn content
      pure (Right ())
    else pure (Left "Todo mot found")

taskStatusToPath :: TaskStatus -> FilePath
taskStatusToPath status = case status of
  Requested -> requestedPath
  Inprogress -> inprogressPath
  Done -> donePath

todoTemaplate :: String
todoTemaplate = unlines ["@title: ", "@author: ", "@implementor: ", "@description", "", "@end"]
