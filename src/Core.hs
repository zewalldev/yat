module Core (doInit, newTodo, startTodo, finishTodo) where

import Data.Bool (bool)
import Entity (TaskName)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, renameFile)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hFlush, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (runCommand, waitForProcess)

yatDir, trackDir, newDir, inprogressDir, doneDir, releaseDir, releasedDir :: FilePath
yatDir = ".yat"
trackDir = "track"
newDir = "new"
inprogressDir = "inprogress"
doneDir = "done"
releaseDir = "release"
releasedDir = "released"

newPath, inprogressPath, donePath, releasePath, releasedPath :: FilePath
newPath = yatDir </> trackDir </> newDir
inprogressPath = yatDir </> trackDir </> inprogressDir
donePath = yatDir </> trackDir </> doneDir
releasePath = yatDir </> trackDir </> releaseDir
releasedPath = yatDir </> trackDir </> releasedDir

alreadyInitialized :: IO ()
alreadyInitialized = putStrLn "Yat already initialized."

notInitialized :: IO ()
notInitialized = putStrLn "Yat not initialized."

findYatRootFrom :: FilePath -> IO (Maybe FilePath)
findYatRootFrom "/" = doesDirectoryExist ("/" </> yatDir) >>= bool (return Nothing) (return . Just $ "/")
findYatRootFrom path = doesDirectoryExist (path </> yatDir) >>= bool (findYatRootFrom . takeDirectory $ path) (return . Just $ path)

findYatRootFromCurrent :: IO (Maybe FilePath)
findYatRootFromCurrent = getCurrentDirectory >>= findYatRootFrom

createYatDirectories :: IO ()
createYatDirectories = do
  createDirectoryIfMissing True newPath
  createDirectoryIfMissing True inprogressPath
  createDirectoryIfMissing True donePath
  createDirectoryIfMissing True releasePath
  createDirectoryIfMissing True releasedPath

editorcmd :: String
editorcmd = "vim"

todoTemaplate :: String
todoTemaplate = unlines ["@title: ", "@author: ", "@implementor: ", "@description", "", "@end"]

createTodo :: FilePath -> IO ()
createTodo todoPath = do
  withSystemTempFile "yat.todo" $ \src hsrc -> do
    hPutStr hsrc todoTemaplate
    hFlush hsrc
    _ <- runCommand (unwords [editorcmd, src]) >>= waitForProcess
    copyFile src todoPath

doInit :: IO ()
doInit = do
  alreadyInit <- doesDirectoryExist yatDir
  if not alreadyInit
    then do
      createYatDirectories
      putStrLn "Yat has initialized."
    else
      alreadyInitialized

newTodo :: TaskName -> IO ()
newTodo name = do
  mb_rootDir <- findYatRootFromCurrent
  case mb_rootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      let new = rootDir </> newPath </> name <.> "todo"
      alreadyEixsts <- doesFileExist new
      if not alreadyEixsts
        then do
          createTodo new
          putStrLn ("Todo " ++ name ++ " is created")
        else
          putStrLn ("Todo " ++ name ++ " already exists")

startTodo :: TaskName -> IO ()
startTodo name = do
  mb_rootDir <- findYatRootFromCurrent
  case mb_rootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      let from = rootDir </> newPath </> name <.> "todo"
      let to = rootDir </> inprogressPath </> name <.> "todo"
      todoExists <- doesFileExist from
      todoInprogress <- doesFileExist to
      if todoExists
        then
          if not todoInprogress
            then do
              renameFile from to
              putStrLn ("Todo " ++ name ++ " is starting")
            else putStrLn "Todo already in progress."
        else putStrLn "Todo doesn't exists"

finishTodo :: TaskName -> IO ()
finishTodo name = do
  mb_rootDir <- findYatRootFromCurrent
  case mb_rootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      let from = rootDir </> inprogressPath </> name <.> "todo"
      let to = rootDir </> donePath </> name <.> "todo"
      todoInProgress <- doesFileExist from
      todoIsDone <- doesFileExist to
      if todoInProgress
        then
          if not todoIsDone
            then do
              renameFile from to
              putStrLn ("Todo " ++ name ++ " is finished")
            else putStrLn "Todo is already done."
        else putStrLn "Todo doesn't exists"
