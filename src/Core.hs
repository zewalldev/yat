module Core (doInit, newTodo, startTodo, finishTodo) where

import Data.Bool (bool)
import Entity (TaskName)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, renameFile)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hFlush, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (createProcess, shell, waitForProcess)

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

findYatRootFromCurrentAnd :: (FilePath -> IO ()) -> IO ()
findYatRootFromCurrentAnd handle = getCurrentDirectory >>= findYatRootFrom >>= maybe notInitialized handle

createYatDirectories :: IO ()
createYatDirectories =
  createDirectoryIfMissing True newPath
    >> createDirectoryIfMissing True inprogressPath
    >> createDirectoryIfMissing True donePath
    >> createDirectoryIfMissing True releasePath
    >> createDirectoryIfMissing True releasedPath
    >> putStrLn "Yat has initialized."

editorcmd :: String
editorcmd = "vim"

todoTemaplate :: String
todoTemaplate = unlines ["@title: ", "@author: ", "@implementor: ", "@description", "", "@end"]

createTodo :: FilePath -> IO ()
createTodo todoPath = do
  withSystemTempFile "yat.todo" $ \src hsrc -> do
    putStrLn src
    hPutStr hsrc todoTemaplate >> hFlush hsrc
    (_, _, _, h) <- createProcess (shell (unwords [editorcmd, src]))
    r <- waitForProcess h
    print r
    copyFile src todoPath

doInit :: IO ()
doInit = doesDirectoryExist yatDir >>= bool createYatDirectories alreadyInitialized

newTodo :: TaskName -> IO ()
newTodo name = findYatRootFromCurrentAnd (createTodo . (</> newPath </> name <.> "todo")) >> putStrLn ("Todo " ++ name ++ " is created")

startTodo :: TaskName -> IO ()
startTodo name = findYatRootFromCurrentAnd $
  \root ->
    renameFile (root </> newPath </> name <.> "todo") (root </> inprogressPath </> name <.> "todo")
      >> putStrLn ("Todo " ++ name ++ " is starting")

finishTodo :: TaskName -> IO ()
finishTodo name = findYatRootFromCurrentAnd $
  \root ->
    renameFile (root </> inprogressPath </> name <.> "todo") (root </> donePath </> name <.> "todo")
      >> putStrLn ("Todo " ++ name ++ " is finished")