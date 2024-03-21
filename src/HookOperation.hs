module HookOperation (runPreHook, runPostHook, Command (..), Entity (..)) where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)
import Types (TaskName)

yatDir, confDir, hooksDir :: FilePath
yatDir = ".yat"
confDir = "conf"
hooksDir = "hooks"

hooksPath :: FilePath
hooksPath = yatDir </> confDir </> hooksDir

data Command = Request | Start | Finish

data Entity = Todo

instance Show Command where
  show Request = "request"
  show Start = "start"
  show Finish = "finish"

instance Show Entity where
  show Todo = "todo"

runPreHook :: FilePath -> Command -> Entity -> TaskName -> IO Bool
runPreHook root command entity name = do
  let scriptPath = root </> hooksPath </> ("pre" ++ "_" ++ show command ++ "_" ++ show entity)
  sriptExists <- doesFileExist scriptPath
  if sriptExists
    then do
      (_, _, _, h) <- createProcess (proc scriptPath [name]) {cwd = Just root}
      exitCode <- waitForProcess h
      pure (exitCode == ExitSuccess)
    else pure True

runPostHook :: FilePath -> Command -> Entity -> TaskName -> IO ()
runPostHook root command entity name = do
  let scriptPath = root </> hooksPath </> ("post" ++ "_" ++ show command ++ "_" ++ show entity)
  sriptExists <- doesFileExist scriptPath
  when sriptExists $ do
      (_, _, _, h) <- createProcess (proc scriptPath [name]) {cwd = Just root}
      _ <- waitForProcess h
      pure ()
