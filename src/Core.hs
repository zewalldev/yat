module Core (doInit, requestTodo, startTodo, finishTodo) where

import Data.Bool (bool)
import Entity (TaskName)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, renameFile)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hFlush, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (CreateProcess (cwd), createProcess, proc, waitForProcess)

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
  createDirectoryIfMissing True requestedPath
  createDirectoryIfMissing True inprogressPath
  createDirectoryIfMissing True donePath
  createDirectoryIfMissing True releasePath
  createDirectoryIfMissing True releasedPath
  createDirectoryIfMissing True hooksPath

editorcmd :: String
editorcmd = "vim"

todoTemaplate :: String
todoTemaplate = unlines ["@title: ", "@author: ", "@implementor: ", "@description", "", "@end"]

createTodo :: FilePath -> IO ()
createTodo todoPath = do
  withSystemTempFile "yat.todo" $ \src hsrc -> do
    hPutStr hsrc todoTemaplate
    hFlush hsrc
    (_, _, _, h) <- createProcess (proc editorcmd [src])
    _ <- waitForProcess h
    copyFile src todoPath

runHook :: String -> String -> String -> String -> String -> IO ExitCode
runHook root stage command entity name = do
  let scriptPath = root </> hooksPath </> (stage ++ "_" ++ command ++ "_" ++ entity)
  sriptExists <- doesFileExist scriptPath
  if sriptExists
    then do
      (_, _, _, h) <- createProcess (proc scriptPath [name]) {cwd = Just root}
      waitForProcess h
    else pure ExitSuccess

doInit :: IO ()
doInit = do
  alreadyInit <- doesDirectoryExist yatDir
  if not alreadyInit
    then do
      createYatDirectories
      putStrLn "Yat has initialized."
    else
      alreadyInitialized

requestTodo :: TaskName -> IO ()
requestTodo name = do
  mb_rootDir <- findYatRootFromCurrent
  case mb_rootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      let new = rootDir </> requestedPath </> name <.> "todo"
      alreadyEixsts <- doesFileExist new
      if not alreadyEixsts
        then do
          ret <- runHook rootDir "pre" "request" "todo" name
          if ret == ExitSuccess
            then do
              createTodo new
              _ <- runHook rootDir "post" "request" "todo" name
              putStrLn ("Todo " ++ name ++ " is created")
            else
              putStrLn "Request is interupted"
        else
          putStrLn ("Todo " ++ name ++ " already exists")

startTodo :: TaskName -> IO ()
startTodo name = do
  mb_rootDir <- findYatRootFromCurrent
  case mb_rootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      let from = rootDir </> requestedPath </> name <.> "todo"
      let to = rootDir </> inprogressPath </> name <.> "todo"
      todoExists <- doesFileExist from
      todoInprogress <- doesFileExist to
      if todoExists
        then
          if not todoInprogress
            then do
              ret <- runHook rootDir "pre" "start" "todo" name
              if ret == ExitSuccess
                then do
                  renameFile from to
                  (_, _, _, h) <- createProcess (proc editorcmd [to])
                  _ <- waitForProcess h
                  _ <- runHook rootDir "post" "start" "todo" name
                  putStrLn ("Todo " ++ name ++ " is starting")
                else
                  putStrLn "Starting is interupted"
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
              ret <- runHook rootDir "pre" "finish" "todo" name
              if ret == ExitSuccess
                then do
                  renameFile from to
                  _ <- runHook rootDir "post" "finish" "todo" name
                  putStrLn ("Todo " ++ name ++ " is finished")
                else
                  putStrLn "Finishing is interupted"
            else putStrLn "Todo is already done."
        else putStrLn "Todo doesn't exists"
