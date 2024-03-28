module TaskParser (parseTodo) where

import Control.Monad (void)
import Data.Functor ((<&>))
import Text.Parsec (char, many, noneOf, oneOf, string)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser, parseFromFile)
import Types (TodoContent (..))

whitespaces :: Parser ()
whitespaces = void (many (oneOf " "))

parserTodo :: Parser TodoContent
parserTodo = do
  _ <- string "@title:"
  _ <- whitespaces
  title <- many (noneOf "\n")
  _ <- char '\n'
  _ <- string "@author:"
  _ <- whitespaces
  author <- many (noneOf "\n")
  _ <- char '\n'
  _ <- string "@implementor:"
  implementor <- many (noneOf "\n")
  _ <- char '\n'
  _ <- string "@description"
  _ <- char '\n'
  description <- manyTill anyChar (try (string "@end"))
  pure
    TodoContent
      { taskTitle = title,
        taskDescription = description,
        taskAuthor = author,
        taskImplementor = implementor
      }

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x

parseTodo :: FilePath -> IO (Maybe TodoContent)
parseTodo fn = parseFromFile parserTodo fn <&> rightToMaybe
