# Logging

A logging type class.

``` {.haskell file=src/Logging.hs}
module Logging where

import Control.Monad.IO.Class
import Control.Monad.Writer
<<import-text>>

data LogLevel = Error | Warning | Message deriving (Show)

class (Monad m) => MonadLogger m where
    logEntry :: LogLevel -> Text -> m ()

instance (Monad m) => MonadLogger (WriterT [(LogLevel, Text)] m) where
    logEntry level msg = tell $ pure (level, msg)

logError :: (MonadLogger m) => Text -> m ()
logError = logEntry Error

logWarning :: (MonadLogger m) => Text -> m ()
logWarning = logEntry Warning

logMessage :: (MonadLogger m) => Text -> m ()
logMessage = logEntry Message

forwardEntries :: (MonadLogger m, Foldable t) => t (LogLevel, Text) -> m ()
forwardEntries = mapM_ (uncurry logEntry)
```

