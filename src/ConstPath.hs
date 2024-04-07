module ConstPath
  ( yatPath,
    trackPath,
    requestedPath,
    inprogressPath,
    donePath,
    confPath,
    hooksPath,
  )
where

import System.FilePath ((</>))

yatPath, trackPath, requestedPath, inprogressPath, donePath, confPath, hooksPath :: FilePath
yatPath = ".yat"
trackPath = yatPath </> "track"
requestedPath = trackPath </> "requested"
inprogressPath = trackPath </> "inprogress"
donePath = trackPath </> "done"
confPath = yatPath </> "conf"
hooksPath = confPath </> "hooks"
