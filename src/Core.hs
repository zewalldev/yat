module Core (doInit, requestTodo, startTodo, finishTodo) where

import Data.Bool (bool)
import EditorOperation (edit)
import HookOperation (Command (..), Entity (..), runPostHook, runPreHook)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import TodoOperation (createTodo, getTodoContent, initialize, isInitilaized, makeTodoCompleted, makeTodoInprogress, setTodoContent, todoTemaplate)
import Types (TaskFullName (..), TaskName, TaskStatus (..))

yatDir :: FilePath
yatDir = ".yat"

notInitialized :: IO ()
notInitialized = putStrLn "Yat not initialized."

findYatRootFrom :: FilePath -> IO (Maybe FilePath)
findYatRootFrom "/" = doesDirectoryExist ("/" </> yatDir) >>= bool (return Nothing) (return . Just $ "/")
findYatRootFrom path = doesDirectoryExist (path </> yatDir) >>= bool (findYatRootFrom . takeDirectory $ path) (return . Just $ path)

findYatRootFromCurrent :: IO (Maybe FilePath)
findYatRootFromCurrent = getCurrentDirectory >>= findYatRootFrom

doInit :: IO ()
doInit = do
  alreadyInit <- isInitilaized
  if alreadyInit
    then do
      putStrLn "Yat already initialized."
    else do
      initialize
      putStrLn "Yat has been initialized."

requestTodo :: TaskName -> IO ()
requestTodo name = do
  mbRootDir <- findYatRootFromCurrent
  case mbRootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      isSuccess <- runPreHook rootDir Request Todo name
      if isSuccess
        then do
          ret1 <- createTodo rootDir name todoTemaplate
          case ret1 of
            Left err -> putStrLn err
            Right _ -> do
              content <- edit todoTemaplate
              ret2 <- setTodoContent rootDir (MkTaskFullName Requested name) content
              case ret2 of
                Left err -> putStrLn err
                Right () -> do
                  runPostHook rootDir Request Todo name
                  putStrLn ("Todo " ++ name ++ " is starting")
        else
          putStrLn "Request is interupted"

startTodo :: TaskName -> IO ()
startTodo name = do
  mbRootDir <- findYatRootFromCurrent
  case mbRootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      isSuccess <- runPreHook rootDir Start Todo name
      if isSuccess
        then do
          mbContent <- getTodoContent rootDir (MkTaskFullName Requested name)
          case mbContent of
            Nothing -> putStrLn "Todo doesn't exists"
            Just content -> do
              newContent <- edit content
              ret1 <- setTodoContent rootDir (MkTaskFullName Requested name) newContent
              case ret1 of
                Left err -> putStrLn err
                Right () -> do
                  ret2 <- makeTodoInprogress rootDir name
                  case ret2 of
                    Left err -> putStrLn err
                    Right _ -> do
                      runPostHook rootDir Start Todo name
                      putStrLn ("Todo " ++ name ++ " is starting")
        else
          putStrLn "Starting is interupted"

finishTodo :: TaskName -> IO ()
finishTodo name = do
  mbRootDir <- findYatRootFromCurrent
  case mbRootDir of
    Nothing -> notInitialized
    Just rootDir -> do
      isSuccess <- runPreHook rootDir Finish Todo name
      if isSuccess
        then do
          ret <- makeTodoCompleted rootDir name
          case ret of
            Left err -> putStrLn err
            Right () -> do
              runPostHook rootDir Finish Todo name
              putStrLn ("Todo " ++ name ++ " is finished")
        else putStrLn "Finishing is interupted"
