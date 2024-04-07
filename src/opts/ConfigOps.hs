module ConfigOps
  ( loadConfig,
    Config (..),
    isInitilaized,
    initialize,
  )
where

import ConstPath (inprogressPath, requestedPath, yatPath, donePath, hooksPath)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (throwE)
import Data.Bool (bool)
import Ops (Op, OpError (OpError))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

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
  if exists then pure () else throwE $ OpError "Yat already initialized."

initialize :: Op ()
initialize = do
  liftIO $ createDirectoryIfMissing True requestedPath
  liftIO $ createDirectoryIfMissing True inprogressPath
  liftIO $ createDirectoryIfMissing True donePath
  liftIO $ createDirectoryIfMissing True hooksPath
