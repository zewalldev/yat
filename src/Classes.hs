module Classes
  ( ToString (..),
    FromStringError (..),
    FromString (..),
    HasTitle (..),
  )
where

class ToString a where
  toString :: a -> String

newtype FromStringError = FromStringError
  { _msg :: String
  }

class FromString a where
  fromString :: String -> Either FromStringError a

class HasTitle a where
  title :: a -> String
