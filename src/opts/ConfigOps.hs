module ConfigOps
  ( loadConfig,
    Config (..),
    isInitilaized,
    initialize,
  )
where

import ConstPath (donePath, hooksPath, inprogressPath, releasePath, releasedPath, requestedPath, yatPath)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (throwE)
import Data.Bool (bool)
import Ops (Op, OpError (OpError))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Control.Monad (when)

findYatRootFrom :: FilePath -> IO (Maybe FilePath)
findYatRootFrom "/" = doesDirectoryExist ("/" </> yatPath) >>= bool (return Nothing) (return . Just $ "/")
findYatRootFrom path = doesDirectoryExist (path </> yatPath) >>= bool (findYatRootFrom . takeDirectory $ path) (return . Just $ path)

findYatRootFromCurrent :: IO (Maybe FilePath)
findYatRootFromCurrent = getCurrentDirectory >>= findYatRootFrom

newtype Config = Config {root :: FilePath}

loadConfig :: Op Config
loadConfig = do
  mbDir <- liftIO findYatRootFromCurrent
  case mbDir of
    Just dir -> pure $ Config {root = dir}
    Nothing -> throwE $ OpError "Yat not initialized."

isInitilaized :: Op ()
isInitilaized = do
  exists <- liftIO $ doesDirectoryExist yatPath
  when exists $ throwE $ OpError "Yat already initialized."

initialize :: Op ()
initialize = do
  liftIO $ createDirectoryIfMissing True requestedPath
  liftIO $ writeFile (requestedPath </> ".hold") ""
  liftIO $ createDirectoryIfMissing True inprogressPath
  liftIO $ writeFile (inprogressPath </> ".hold") ""
  liftIO $ createDirectoryIfMissing True donePath
  liftIO $ writeFile (donePath </> ".hold") ""
  liftIO $ createDirectoryIfMissing True releasePath
  liftIO $ writeFile (releasePath </> ".hold") ""
  liftIO $ createDirectoryIfMissing True releasedPath
  liftIO $ writeFile (releasedPath </> ".hold") ""
  liftIO $ createDirectoryIfMissing True hooksPath
  liftIO $ writeFile (hooksPath </> ".hold") ""
