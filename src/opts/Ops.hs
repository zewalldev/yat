module Ops (runOp, Op, OpError (..)) where

import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Function ((&))
import Data.String (IsString (fromString))
import Rainbow (fore, putChunkLn, red)

newtype OpError = OpError {errMsg :: String}

type Op a = ExceptT OpError IO a

runOp :: Op String -> IO ()
runOp = runExceptT >=> report

printError :: String -> IO ()
printError msg = putChunkLn $ fromString msg & fore red

printOk :: String -> IO ()
printOk = putStr

report :: Either OpError String -> IO ()
report = either (printError . errMsg) printOk
