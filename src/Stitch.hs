-- ~\~ language=Haskell filename=src/Stitch.hs
-- ~\~ begin <<lit/14-stitch.md|src/Stitch.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Stitch where

import RIO hiding (some)
import qualified RIO.Text as T

-- ~\~ begin <<lit/14-stitch.md|stitch-imports>>[0]
import ListStream (ListStream(..), tokenP)
import Document
import Config (config, HasConfig, Config, languageFromName, ConfigLanguage(..))
import Comment (topHeader, beginBlock, endBlock, commented)
import TextUtil (indent, unindent, unlines')

import Text.Megaparsec
    ( MonadParsec, Parsec, parse, anySingle, manyTill, some, errorBundlePretty )
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[0]
sourceDocument :: ( MonadParsec e (ListStream Text) m
                  , MonadFail m
                  , MonadReader Config m )
               => m [ReferencePair]
sourceDocument = do
    cfg <- ask
    (prop, _) <- tokenP topHeader
    lang <- maybe (fail "No valid language found in header.") return
                  $ getAttribute prop "language" >>= languageFromName cfg
    (_, refs) <- mconcat <$> some (sourceBlock lang)
    return refs
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[1]
sourceBlock :: ( MonadParsec e (ListStream Text) m, MonadFail m )
            => ConfigLanguage -> m ([Text], [ReferencePair])
sourceBlock lang = do
    ((ref, beginIndent), _) <- tokenP (commented lang beginBlock)
    (ilines, refpairs) <- mconcat <$> manyTill
                (sourceBlock lang <|> sourceLine)
                (tokenP (commented lang endBlock))
    let unindentedLines = map (unindent beginIndent) ilines
    when (any isNothing unindentedLines) $ fail "Indentation error"
    let content = unlines' $ catMaybes unindentedLines
    return ( [ indent beginIndent $ showNowebReference $ referenceName ref
             | referenceCount ref == 0 ]
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
    either (throwM . StitchError . T.pack . errorBundlePretty)
           return $ parse doc file $ ListStream (T.lines text)
-- ~\~ end
-- ~\~ end
