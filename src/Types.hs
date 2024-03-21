module Types (TaskName, TaskFullName(..), TaskStatus(..)) where

type TaskName = String

data TaskStatus = Requested | Inprogress | Done

data TaskFullName = MkTaskFullName TaskStatus TaskName