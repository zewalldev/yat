module EditorOperation (edit) where

import System.IO (hFlush, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (createProcess, proc, waitForProcess)
import GHC.IO.Handle (hClose)

editorcmd :: String
editorcmd = "vim"

edit :: String -> IO String
edit content = withSystemTempFile "yat" $ \src hsrc -> do
  hPutStr hsrc content
  hFlush hsrc
  (_, _, _, h) <- createProcess (proc editorcmd [src])
  _ <- waitForProcess h
  hClose hsrc
  readFile src
