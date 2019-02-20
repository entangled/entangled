{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Console
    ( ConsoleT
    , run
    , Info(..)
    , HyperElement(..)
    , HyperText
    , HyperTextable
    , message
    , errorMessage
    , warning
    , indent
    , unindent ) where

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.IO.Class

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Prettyprint.Doc as P
import qualified System.Console.Terminal.Size as Terminal

-- ==== Pretty Printing document tree ==== --

data LogLevel = Error | Warning | Message deriving (Show)
data FileAction = OverWrite | Delete | Create deriving (Show)

data Annotation
    = Emphasise
    | Header
    | File FileAction
    | Log LogLevel
    deriving (Show)

type Doc = P.Doc Annotation

log :: P.Pretty a => LogLevel -> a -> Doc
log level = annotate (Log level) . pretty

message :: P.Pretty a => a -> Doc
message = logMessage Message

-- ==== Pretty Printing document to console ==== --

data Info = Info
    { consoleSize    :: Terminal.Window Int
    , consolePalette :: Map Text Text
    } deriving (Show)

newtype ConsoleT m a = ConsoleT {
    runConsole :: ReaderT Info m a
} deriving (Applicative, Functor, Monad, MonadReader Info, MonadTrans)

getColour :: MonadReader Info m => ColourName -> m Text
getColour n = reader (M.findWithDefault "" n . consolePalette)

initInfoLinux :: IO Info
initInfoLinux = do
    size <- fromMaybe (Terminal.Window 24 80) <$> Terminal.size
    let palette = M.fromList
            [ ("decoration", "\ESC[38m")
            , ("reset",   "\ESC[m")
            , ("error",   "\ESC[31m")
            , ("warning", "\ESC[33m")
            , ("message", "\ESC[m")
            ]
    return $ Info size palette

run :: MonadIO m => ConsoleT m a -> m a
run x = liftIO initInfoLinux >>= evalStateT (runConsole x)

