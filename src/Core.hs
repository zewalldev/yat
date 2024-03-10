module Core (doInit) where

import Data.Bool (bool)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

yatDir, trackDir, newDir, inprogressDir, doneDir, releaseDir, releasedDir :: FilePath
yatDir = ".yat"
trackDir = "track"
newDir = "new"
inprogressDir = "inprogressPath"
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

createYatDirectories :: IO ()
createYatDirectories =
  createDirectoryIfMissing True newPath
    >> createDirectoryIfMissing True inprogressPath
    >> createDirectoryIfMissing True donePath
    >> createDirectoryIfMissing True releasePath
    >> createDirectoryIfMissing True releasedPath
    >> putStrLn "Yat has initialized."

doInit :: IO ()
doInit = doesDirectoryExist yatDir >>= bool createYatDirectories alreadyInitialized