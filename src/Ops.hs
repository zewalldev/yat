module Ops (runOp, Op, OpError (..)) where

import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Rainbow (putChunkLn, fore, red)
import Data.String (IsString(fromString))
import Data.Function ((&))

newtype OpError = OpError {errMsg :: String}

type Op a = ExceptT OpError IO a

runOp :: (Show a) => Op a -> IO ()
runOp = runExceptT >=> report

printError :: String -> IO ()
printError msg = putChunkLn $ fromString msg & fore red
printOk :: Show a => a -> IO ()
printOk = print 

report :: (Show a) => Either OpError a -> IO ()
report = either (printError . errMsg) printOk
