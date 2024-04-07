module TaskParser
  ( requestedTodoParser,
    inprogressTodoParser,
    doneTodoParser,
  )
where

import ParserUtils (tag, content)
import Text.Parsec.String (Parser)
import Types (Done (..), Inprogress (..), Requested (..), Task (..))

requestedTodoParser :: Parser Requested
requestedTodoParser = do
  title <- tag "title"
  author <- tag "author"
  description <- content "description"
  pure
    Requested
      { _task =
          Todo
            { _title = title,
              _description = description
            },
        _author = author
      }

inprogressTodoParser :: Parser Inprogress
inprogressTodoParser = do
  title <- tag "title"
  author <- tag "author"
  implementer <- tag "implementer"
  description <- content "description"
  pure
    Inprogress
      { _implementer = implementer,
        _requested =
          Requested
            { _author = author,
              _task =
                Todo
                  { _title = title,
                    _description = description
                  }
            }
      }

doneTodoParser :: Parser Done
doneTodoParser = do
  title <- tag "title"
  author <- tag "author"
  implementer <- tag "implementer"
  description <- content "description"
  pure
    Done
      { _inprogress =
          Inprogress
            { _implementer = implementer,
              _requested =
                Requested
                  { _author = author,
                    _task =
                      Todo
                        { _title = title,
                          _description = description
                        }
                  }
            }
      }
