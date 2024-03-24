module ConfigOps (loadConfig, Config (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (throwE)
import Data.Bool (bool)
import Ops (Op, OpError (OpError))
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

yatDir :: FilePath
yatDir = ".yat"

findYatRootFrom :: FilePath -> IO (Maybe FilePath)
findYatRootFrom "/" = doesDirectoryExist ("/" </> yatDir) >>= bool (return Nothing) (return . Just $ "/")
findYatRootFrom path = doesDirectoryExist (path </> yatDir) >>= bool (findYatRootFrom . takeDirectory $ path) (return . Just $ path)

findYatRootFromCurrent :: IO (Maybe FilePath)
findYatRootFromCurrent = getCurrentDirectory >>= findYatRootFrom

newtype Config = Config {root :: FilePath}

loadConfig :: Op Config
loadConfig = do
  mbDir <- liftIO findYatRootFromCurrent
  case mbDir of
    Just dir -> pure $ Config {root = dir}
    Nothing -> throwE $ OpError "Yat not initialized."