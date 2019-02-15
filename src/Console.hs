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

import qualified System.Console.Terminal.Size as Terminal

data HyperElement
    = Content Text
    | Colour  Text
    deriving (Show)

type HyperText = [HyperElement]
type ColourName = Text

data Info = Info
    { consoleSize    :: Terminal.Window Int
    , consolePalette :: Map Text Text
    , consoleIndent  :: [HyperText]
    } deriving (Show)

newtype ConsoleT m a = ConsoleT {
    runConsole :: StateT Info m a
} deriving (Applicative, Functor, Monad, MonadIO, MonadState Info, MonadTrans)
-- type ConsoleIO = ConsoleT IO

class HyperTextable x where
    toHyperText :: x -> HyperText

instance HyperTextable String where
    toHyperText s = [Content $ T.pack s]

instance HyperTextable Text where
    toHyperText t = [Content t]

instance HyperTextable HyperText where
    toHyperText = id

getColour :: MonadState Info m => ColourName -> m Text
getColour n = gets (M.findWithDefault "" n . consolePalette)

getText :: MonadState Info m => HyperElement -> m Text
getText (Content t) = return t
getText (Colour t) = getColour t

toText :: MonadState Info m => HyperText -> m Text
toText ht = do
    ts <- mapM getText ht
    return $ mconcat ts

printedLength :: HyperText -> Int
printedLength (Colour _  : xs) = printedLength xs
printedLength (Content x : xs) = T.length x + printedLength xs

hyperPrint :: (MonadState Info m, MonadIO m) => HyperText -> m ()
hyperPrint ht = do
    t <- toText ht
    liftIO $ putStrLn $ T.unpack t

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
    return $ Info size palette []

defaultHead :: a -> [a] -> a
defaultHead d []     = d
defaultHead d (x:xs) = x

defaultTail :: [a] -> [a] -> [a]
defaultTail d []     = d
defaultTail d (x:xs) = xs

indent :: (HyperTextable a, MonadState Info m) => a -> m ()
indent x = modify $ addIndent $ toHyperText x
    where addIndent i info = info
            { consoleIndent = i : consoleIndent info }

unindent :: MonadState Info m => m ()
unindent = modify remIndent
    where remIndent info = info
            { consoleIndent = defaultTail [] (consoleIndent info) }

prefix :: Info -> HyperText
prefix = mconcat . reverse . consoleIndent

message :: (HyperTextable a, MonadIO m, MonadState Info m) => a -> m ()
message t = do
    indent <- gets prefix
    hyperPrint $ indent <> [ Colour "message" ]
                        <> toHyperText t
                        <> [ Colour "reset" ]

errorMessage :: (HyperTextable a, MonadIO m, MonadState Info m) => a -> m ()
errorMessage t = do
    indent <- gets prefix
    hyperPrint $ indent <> [ Colour "error" ]
                        <> toHyperText t
                        <> [ Colour "reset" ]

warning :: (HyperTextable a, MonadIO m, MonadState Info m) => a -> m ()
warning t = do
    indent <- gets prefix
    hyperPrint $ indent <> [ Colour "warning" ]
                        <> toHyperText t
                        <> [ Colour "reset" ]

run :: MonadIO m => ConsoleT m a -> m a
run x = liftIO initInfoLinux >>= evalStateT (runConsole x)
