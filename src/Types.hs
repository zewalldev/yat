{-# LANGUAGE RankNTypes #-}

module Types (TaskName, TaskFullName (..), TaskStatus (..), TodoContent(..)) where

type TaskName = String

data TaskStatus = Requested | Inprogress | Done deriving (Show)

data TaskFullName = MkTaskFullName TaskStatus TaskName

data TodoContent = TodoContent {
    taskTitle :: String,
    taskDescription :: String,
    taskAuthor :: String,
    taskImplementor :: String
}

