module HookOps (runPreHook, runPostHook, HookCommand (..), Entity (..)) where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)
import Types (TaskName)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.IO.Class (liftIO)
import Ops (Op, OpError (OpError))

yatDir, confDir, hooksDir :: FilePath
yatDir = ".yat"
confDir = "conf"
hooksDir = "hooks"

hooksPath :: FilePath
hooksPath = yatDir </> confDir </> hooksDir

data HookCommand = Request | Start | Finish

data Entity = Todo

instance Show HookCommand where
  show Request = "request"
  show Start = "start"
  show Finish = "finish"

instance Show Entity where
  show Todo = "todo"


runPreHook :: FilePath -> HookCommand -> Entity -> TaskName -> Op ()
runPreHook root command entity name = do
  let scriptPath = root </> hooksPath </> ("pre" ++ "_" ++ show command ++ "_" ++ show entity)
  sriptExists <-  liftIO . doesFileExist $ scriptPath
  when sriptExists $ do
      (_, _, _, h) <- liftIO . createProcess $ (proc scriptPath [name]) {cwd = Just root}
      exitCode <- liftIO . waitForProcess $ h
      when (exitCode /= ExitSuccess) $ throwE $ OpError "Error"

runPostHook :: FilePath -> HookCommand -> Entity -> TaskName -> Op ()
runPostHook root command entity name = do
  let scriptPath = root </> hooksPath </> ("post" ++ "_" ++ show command ++ "_" ++ show entity)
  sriptExists <- liftIO . doesFileExist $ scriptPath
  when sriptExists $ do
      (_, _, _, h) <- liftIO . createProcess $ (proc scriptPath [name]) {cwd = Just root}
      _ <- liftIO . waitForProcess $ h
      pure ()