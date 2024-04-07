{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types
  ( TaskStatus (..),
    Task (..),
    Requested (..),
    Inprogress (..),
    Done (..),
    TaskKey,
    Command (..),
    OperationCommand (..),
  )
where

data TaskStatus = RequestedStatus | InprogressStatus | DoneStatus deriving (Show)

type TaskKey = String

data Task = Todo
  { _title :: String,
    _description :: String
  }

data Requested = Requested
  { _task :: Task,
    _author :: String
  }

data Inprogress = Inprogress
  { _requested :: Requested,
    _implementer :: String
  }

data Done = Done
  { _inprogress :: Inprogress
  }

data Command
  = InitCommand
  | RequestTaskCommand TaskKey
  | StartTaskCommand TaskKey
  | FinishTaskCommand TaskKey
  | ListTaskCommand TaskStatus
  deriving (Show)

data OperationCommand = RequestOperation | StartOperation | FinishOperation
