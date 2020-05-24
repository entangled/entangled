-- ~\~ language=Haskell filename=src/Stitch.hs
-- ~\~ begin <<lit/14-stitch.md|src/Stitch.hs>>[0]
module Stitch where

import RIO (view)
-- ~\~ begin <<lit/14-stitch.md|stitch-imports>>[0]
import ListStream (ListStream(..), tokenP)
import Document
import Config (config, HasConfig, Config, languageFromName, ConfigLanguage(..))
import Comment (topHeader, beginBlock, endBlock, commented)
import TextUtil (indent, unindent, unlines')

import Text.Megaparsec
    ( MonadParsec, Parsec, parse, anySingle, manyTill, (<|>)
    , many, some, errorBundlePretty )
import Data.Void (Void)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Catch (MonadThrow, throwM)
-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import RIO (Text)
import qualified RIO.Text as T
-- ~\~ end
import qualified Data.Map.Strict as M
import Control.Monad.Reader (MonadReader, ask, asks, ReaderT, runReaderT)
import Control.Monad (when)
import Data.Maybe (isNothing, catMaybes)
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[0]
sourceDocument :: ( MonadParsec e (ListStream Text) m
                  , MonadFail m
                  , MonadReader Config m )
               => m [ReferencePair]
sourceDocument = do
    config <- ask
    (prop, _) <- tokenP topHeader
    lang <- maybe (fail "No valid language found in header.") return
                  $ getAttribute prop "language" >>= languageFromName config
    (_, refs) <- mconcat <$> some (sourceBlock lang)
    return refs
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[1]
sourceBlock :: ( MonadParsec e (ListStream Text) m, MonadFail m )
            => ConfigLanguage -> m ([Text], [ReferencePair])
sourceBlock lang = do
    ((ref, beginIndent), _) <- tokenP (commented lang beginBlock)
    (lines, refpairs) <- mconcat <$> manyTill
                (sourceBlock lang <|> sourceLine)
                (tokenP (commented lang endBlock))
    let unindentedLines = map (unindent beginIndent) lines
    when (any isNothing unindentedLines) $ fail "Indentation error"
    let content = unlines' $ catMaybes unindentedLines
    return ( if referenceCount ref == 0
                 then [(indent beginIndent $ showNowebReference $ referenceName ref)]
                 else []
           , (ref, CodeBlock (KnownLanguage $ languageName lang) [] content):refpairs )

sourceLine :: ( MonadParsec e (ListStream Text) m )
           => m ([Text], [ReferencePair])
sourceLine = do
    x <- anySingle
    return ([x], [])
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|stitch>>[0]
type SourceParser = ReaderT Config (Parsec Void (ListStream Text))

untangle :: ( MonadReader env m, HasConfig env, MonadThrow m )
         => FilePath -> Text -> m [ReferencePair]
untangle file text = do
    doc <- runReaderT (sourceDocument :: SourceParser [ReferencePair]) <$> view config
    either (\err -> throwM $ StitchError $ T.pack $ errorBundlePretty err)
           return $ parse doc file $ ListStream (T.lines text)
-- ~\~ end
-- ~\~ end
