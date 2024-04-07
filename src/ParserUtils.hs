module ParserUtils (identifier, spaces1, tag, content) where

import Text.Parsec (anyChar, char, letter, manyTill, noneOf, oneOf, space, spaces, string, try, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (many)
import Text.Parsec.String (Parser)

identifier :: Parser String
identifier = do
  h <- firstChar
  t <- many nonFirstChar
  return (h : t)
  where
    firstChar = letter
    nonFirstChar = digit <|> firstChar <|> char '-'

spaces1 :: Parser String
spaces1 = many1 space

tag :: String -> Parser String
tag name = do
  _ <- string $ "@" ++ name ++ ":"
  _ <- spaces
  value <- many (noneOf "\n")
  _ <- char '\n'
  pure value

content :: String -> Parser String
content name = do
  _ <- string $ "@" ++ name
  _ <- char '\n'
  value <- manyTill anyChar (try (string "@end"))
  _ <- many (oneOf "\n")
  pure value
