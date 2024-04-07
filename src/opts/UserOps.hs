module UserOps (getUser) where

import Control.Monad.IO.Class (liftIO)
import Ops (Op)
import System.Process (readProcess)

getUserName :: Op String
getUserName = do
  out <- liftIO . readProcess "git" ["config", "user.name"] $ ""
  pure (filter (/= '\n') out)

getUserEmail :: Op String
getUserEmail = do
  out <- liftIO . readProcess "git" ["config", "user.email"] $ ""
  pure (filter (/= '\n') out)

getUser :: Op String
getUser = do
  username <- getUserName
  email <- getUserEmail
  pure (username ++ " <" ++ email ++ ">")
