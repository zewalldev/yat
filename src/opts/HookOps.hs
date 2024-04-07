module HookOps
  ( runPreHook,
    runPostHook,
  )
where

import ConstPath (hooksPath)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Ops (Op, OpError (OpError))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)
import Types (Identifier (..), OperationCommand (..))

op2string :: OperationCommand -> String
op2string RequestOperation = "request"
op2string StartOperation = "start"
op2string FinishOperation = "finish"

id2string :: Identifier -> (String, String)
id2string (TodoKey key) = ("todo", key)
id2string (ReleaseKey version) = ("release", version)

runPreHook :: FilePath -> OperationCommand -> Identifier -> Op ()
runPreHook root command ind = do
  let (entity, name) = id2string ind
  let scriptPath = root </> hooksPath </> ("pre" ++ "_" ++ op2string command ++ "_" ++ entity)
  sriptExists <- liftIO . doesFileExist $ scriptPath
  when sriptExists $ do
    (_, _, _, h) <- liftIO . createProcess $ (proc scriptPath [name]) {cwd = Just root}
    exitCode <- liftIO . waitForProcess $ h
    when (exitCode /= ExitSuccess) $ throwE $ OpError "Error"

runPostHook :: FilePath -> OperationCommand -> Identifier -> Op ()
runPostHook root command ind = do
  let (entity, name) = id2string ind
  let scriptPath = root </> hooksPath </> ("post" ++ "_" ++ op2string command ++ "_" ++ entity)
  sriptExists <- liftIO . doesFileExist $ scriptPath
  when sriptExists $ do
    (_, _, _, h) <- liftIO . createProcess $ (proc scriptPath [name]) {cwd = Just root}
    _ <- liftIO . waitForProcess $ h
    pure ()
