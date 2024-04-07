module ConstPath
  ( yatPath,
    trackPath,
    requestedPath,
    inprogressPath,
    releasePath,
    releasedPath,
    donePath,
    confPath,
    hooksPath,
  )
where

import System.FilePath ((</>))

yatPath, trackPath, requestedPath, inprogressPath, releasePath, releasedPath, donePath, confPath, hooksPath :: FilePath
yatPath = ".yat"
trackPath = yatPath </> "track"
requestedPath = trackPath </> "requested"
inprogressPath = trackPath </> "inprogress"
releasePath = trackPath </> "release"
releasedPath = trackPath </> "released"
donePath = trackPath </> "done"
confPath = yatPath </> "conf"
hooksPath = confPath </> "hooks"
