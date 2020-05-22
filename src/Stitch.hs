-- ------ language="Haskell" file="src/Stitch.hs" project://src/Stitch.hs#2
module Stitch where

import RIO (view)
-- ------ begin <<stitch-imports>>[0] project://src/Stitch.hs#3
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
-- ------ begin <<import-text>>[0] project://lit/01-entangled.md
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Map.Strict as M
import Control.Monad.Reader (MonadReader, ask, asks, ReaderT, runReaderT)
import Control.Monad (when)
import Data.Maybe (isNothing, catMaybes)
-- ------ end
-- ------ begin <<source-parser>>[0] project://lit/14-stitch.md
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
-- ------ end
-- ------ begin <<source-parser>>[1] project://lit/14-stitch.md
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
-- ------ end
-- ------ begin <<stitch>>[0] project://src/Stitch.hs#11
type SourceParser = ReaderT Config (Parsec Void (ListStream Text))

untangle :: ( MonadReader env m, HasConfig env, MonadThrow m )
         => FilePath -> Text -> m [ReferencePair]
untangle file text = do
    doc <- runReaderT (sourceDocument :: SourceParser [ReferencePair]) <$> view config
    either (\err -> throwM $ StitchError $ T.pack $ errorBundlePretty err)
           return $ parse doc file $ ListStream (T.lines text)

stitch :: ( MonadReader Config m )
         => FilePath -> Text
         -> m (Either EntangledError [ReferencePair])
stitch filename text = do
    p <- asks $ runReaderT (sourceDocument :: SourceParser [ReferencePair])
    let refs = parse p filename $ ListStream (T.lines text)
    return $ either (\e -> Left $ StitchError $ T.pack $ errorBundlePretty e)
                    Right refs
-- ------ end
-- ------ end
