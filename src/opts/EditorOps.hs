module EditorOps (edit) where

import Control.Monad.IO.Class (liftIO)
import GHC.IO.Handle (hClose)
import Ops (Op)
import System.IO (hFlush, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (createProcess, proc, waitForProcess)

editorcmd :: String
editorcmd = "vim"

edit :: String -> Op String
edit content = withSystemTempFile "yat" $ \src hsrc -> do
  liftIO . hPutStr hsrc $ content
  liftIO . hFlush $ hsrc
  (_, _, _, h) <- liftIO . createProcess $ proc editorcmd [src]
  _ <- liftIO $ waitForProcess h
  liftIO . hClose $ hsrc
  liftIO . readFile $ src
