-- ~\~ language=Haskell filename=src/Stitch.hs
-- ~\~ begin <<lit/14-stitch.md|src/Stitch.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Stitch where

import RIO hiding (some)
import qualified RIO.Text as T
import RIO.List.Partial (head, tail)

-- ~\~ begin <<lit/14-stitch.md|stitch-imports>>[0]
import ListStream (ListStream(..), tokenP)
import Document
import Config (config, HasConfig, Config(..), languageFromName, ConfigLanguage(..))
import Comment (topHeader, beginBlock, endBlock, commented)
import TextUtil (indent, unindent, unlines')

import Text.Megaparsec
    ( MonadParsec, Parsec, parse, anySingle, manyTill, some, errorBundlePretty, manyTill_ )
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[0]
sourceDocument :: ( MonadParsec e (ListStream Text) m
                  , MonadFail m
                  , MonadReader Config m )
               => m [ReferencePair]
sourceDocument = do
    cfg <- ask
    (header, (prop, _)) <- manyTill_ anySingle (tokenP topHeader)
    -- (prop, _) <- tokenP topHeader
    lang <- maybe (fail "No valid language found in header.") return
                  $ getAttribute prop "language" >>= languageFromName cfg
    (_, refs) <- mconcat <$> some (sourceBlock lang)
    return (prependCode header (head refs) : tail refs)
    where prependCode h (ref, cb@CodeBlock{..})
            = (ref, cb {codeSource = unlines' (h <> [codeSource])})
-- ~\~ end
-- ~\~ begin <<lit/14-stitch.md|source-parser>>[1]
sourceBlock :: ( MonadReader Config m, MonadParsec e (ListStream Text) m, MonadFail m )
            => ConfigLanguage -> m ([Text], [ReferencePair])
sourceBlock lang = do
    Config{..} <- ask
    ((ref, beginIndent), _) <- tokenP (commented lang beginBlock)
    when configUseLineDirectives (void anySingle)
    (ilines, refpairs) <- mconcat <$> manyTill
                (sourceBlock lang <|> sourceLine)
                (tokenP (commented lang endBlock))
    let unindentedLines = map (unindent beginIndent) ilines
    when (any isNothing unindentedLines) $ fail "Indentation error"
    let content = unlines' $ catMaybes unindentedLines
    return ( [ indent beginIndent $ showNowebReference $ referenceName ref
             | referenceCount ref == 0 ]
           , (ref, CodeBlock (KnownLanguage $ languageName lang) [] content Nothing):refpairs )

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
