module Console
    ( msg
    , Doc
    , FileAction(..)
    , putTerminal
    , msgDelete
    , msgWrite
    , msgCreate
    , fileRead
    , group
    , banner
    , bullet
    , timeStamp
    ) where

import Control.Monad.Reader
-- import Control.Monad.State.Class
-- import Control.Monad.IO.Class

-- import Data.Maybe (fromMaybe)
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Text.IO as T
-- import Data.Map (Map)
-- import qualified Data.Map as M
import qualified Data.Text.Prettyprint.Doc as P
-- import qualified System.Console.Terminal.Size as Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as ANSI
import qualified System.Info
import Data.Time
import RIO (LogLevel(..))

-- ==== Pretty Printing document tree ==== --

data FileAction = Read | Write | Delete | Create deriving (Show)

data Annotation
    = Emphasise
    | Header
    | Decoration
    | TimeStamp
    | File FileAction
    | Log LogLevel
    deriving (Show)

type Doc = P.Doc Annotation

banner :: Doc
banner =  P.annotate Emphasise "Entangled"
       <> ", version 1.0.0: https://entangled.github.io/"
       <> P.line <> P.line

msg :: P.Pretty a => LogLevel -> a -> Doc
msg level = P.annotate (Log level) . P.pretty

timeStamp :: MonadIO m => m Doc
timeStamp = do
    t <- liftIO getZonedTime
    return $ P.annotate TimeStamp
           $ "[" <> P.pretty (formatTime defaultTimeLocale "%T" t)
           <> "]"

bullet :: Doc -> Doc
bullet = (P.annotate Decoration bulletChar P.<+>)
    where bulletChar = if System.Info.os == "linux" then "•" else "*"

group :: Doc -> Doc -> Doc
group h d = bullet (P.annotate Header h) <> P.line <> P.indent 4 d <> P.line

msgWrite :: FilePath -> Doc
msgWrite f = bullet "Writing"
    P.<+> P.annotate (File Write) (P.squotes $ P.pretty f)
    <> P.line

msgCreate :: FilePath -> Doc
msgCreate f = bullet "Creating"
    P.<+> P.annotate (File Create) (P.squotes $ P.pretty f)
    <> P.line

msgDelete :: FilePath -> Doc
msgDelete f = bullet "Deleting"
    P.<+> P.annotate (File Delete) (P.squotes $ P.pretty f)
    <> P.line

fileRead :: FilePath -> Doc
fileRead f = P.annotate (File Read) $ P.squotes $ P.pretty f

toTerminal :: Doc -> P.SimpleDocStream ANSI.AnsiStyle
toTerminal d = P.reAnnotateS tr $ P.layoutPretty P.defaultLayoutOptions d
    where tr Emphasise = ANSI.bold
          tr Header = ANSI.bold <> ANSI.color ANSI.White
          tr Decoration = ANSI.colorDull ANSI.Blue
          tr TimeStamp = ANSI.colorDull ANSI.Blue
          tr (File Read) = ANSI.color ANSI.White <> ANSI.italicized
          tr (File Write) = ANSI.color ANSI.Yellow <> ANSI.italicized
          tr (File Delete) = ANSI.color ANSI.Red <> ANSI.italicized
          tr (File Create) = ANSI.color ANSI.Blue <> ANSI.italicized
          tr (Log LevelError) = ANSI.color ANSI.Red <> ANSI.bold
          tr (Log LevelWarn) = ANSI.color ANSI.Yellow <> ANSI.bold
          tr (Log _) = ANSI.colorDull ANSI.White

putTerminal :: Doc -> IO ()
putTerminal = T.putStr . ANSI.renderStrict . toTerminal

