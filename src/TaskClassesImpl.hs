{-# OPTIONS_GHC -Wno-orphans #-}

module TaskClassesImpl () where

import Classes (HasTitle (..), ToString (..), FromString (..), FromStringError (..))
import Types (Done (..), Inprogress (..), Requested (..), Task (..))
import Control.Arrow (left)
import Text.Parsec (runParser)
import TaskParser (requestedTodoParser, inprogressTodoParser, doneTodoParser)

instance HasTitle Requested where
  title = _title . _task

instance HasTitle Inprogress where
  title = title . _requested

instance HasTitle Done where
  title = title . _inprogress

instance ToString Requested where
  toString
    Requested
      { _task =
          Todo
            { _title = _title,
              _description = description
            },
        _author = author
      } =
      unlines
        [ "@title: " ++ _title,
          "@author: " ++ author,
          "@description",
          description,
          "@end"
        ]

instance ToString Inprogress where
  toString
    Inprogress
      { _implementer = implementer,
        _requested =
          Requested
            { _task =
                Todo
                  { _title = _title,
                    _description = description
                  },
              _author = author
            }
      } =
      unlines
        [ "@title: " ++ _title,
          "@author: " ++ author,
          "@implementer: " ++ implementer,
          "@description",
          description,
          "@end"
        ]

instance ToString Done where
  toString
    Done
      { _inprogress =
          Inprogress
            { _implementer = implementer,
              _requested =
                Requested
                  { _task =
                      Todo
                        { _title = _title,
                          _description = description
                        },
                    _author = author
                  }
            }
      } =
      unlines
        [ "@title: " ++ _title,
          "@author: " ++ author,
          "@implementer: " ++ implementer,
          "@description",
          description,
          "@end"
        ]

instance FromString Requested where
  fromString = left (FromStringError . show) . runParser requestedTodoParser () ""

instance FromString Inprogress where
  fromString = left (FromStringError . show) . runParser inprogressTodoParser () ""

instance FromString Done where
  fromString = left (FromStringError . show) . runParser doneTodoParser () ""
