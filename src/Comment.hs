-- ~\~ language=Haskell filename=src/Comment.hs
-- ~\~ begin <<lit/13-tangle.md|src/Comment.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Comment where

import RIO
import qualified RIO.Text as T

-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[0]
import Control.Monad.Except

-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[1]
import Config
    ( Config(..), ConfigLanguage(..), ConfigComment(..), languageFromName )
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[2]
import qualified RIO.Map as M

import Document
import TextUtil (unlines')
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[3]
import Text.Megaparsec            ( MonadParsec, chunk, skipManyTill
                                  , anySingle, (<?>), takeWhileP, eof, takeRest )
import Text.Megaparsec.Char       ( space )
import Text.Megaparsec.Char.Lexer ( decimal )

import Attributes (attributes, cssIdentifier, cssValue)
-- ~\~ end

-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[0]
delim :: Text
delim = " ~\\~ "
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[1]
comment :: (MonadReader Config m, MonadError EntangledError m)
        => ProgrammingLanguage
        -> Text
        -> m Text
comment (UnknownClass cls) _ = throwError $ UnknownLanguageClass cls
comment NoLanguage         _ = throwError MissingLanguageClass
comment (KnownLanguage langName) text = do
    cfg <- ask
    maybe (throwError $ SystemError $ "language named " <> langName <> " is not in config.")
          (\lang -> return $ formatComment lang text)
          (languageFromName cfg langName)

commentStart :: ConfigComment -> Text
commentStart (Block x _) = x
commentStart (Line x) = x

commentEnd :: ConfigComment -> Maybe Text
commentEnd (Block _ x) = Just x
commentEnd (Line _) = Nothing

formatComment :: ConfigLanguage -> Text -> Text
formatComment lang text = pre <> text <> post
    where pre  = commentStart (languageComment lang) <> delim
          post = maybe "" (" " <>) $ commentEnd $ languageComment lang
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[2]
standardPreComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceId -> CodeBlock -> m Text
standardPreComment (ReferenceId file (ReferenceName name) count) code = comment (codeLanguage code)
    $ "begin <<" <> T.pack file <> "|" <> name <> ">>[" <> tshow count <> "]"

getReference :: (MonadError EntangledError m) => ReferenceMap -> ReferenceId -> m CodeBlock
getReference refs ref = maybe (throwError $ ReferenceError $ "not found: " <> tshow ref)
                              return (refs M.!? ref)

annotateProject :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> ReferenceId -> m Text
annotateProject refs ref@(ReferenceId file _ _) = do
    code <- getReference refs ref
    let line = fromMaybe 0 (codeLineNumber code)
    pre  <- (<> " project://" <> T.pack file <> "#" <> tshow line)
         <$> standardPreComment ref code
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, codeSource code, post]

annotateComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> ReferenceId -> m Text
annotateComment refs ref = do
    Config{..} <- ask
    code <- getReference refs ref
    pre <- standardPreComment ref code
    lineDirective <- comment (codeLanguage code) "line directives are not yet implemented"
    post <- comment (codeLanguage code) "end"
    return $ if configUseLineDirectives
             then unlines' [pre, lineDirective, codeSource code, post]
             else unlines' [pre, codeSource code, post]

headerComment :: ConfigLanguage -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[0]
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = skipManyTill (anySingle <?> "open comment")
                         (chunk delim)
          >> attributes
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[1]
commented :: (MonadParsec e Text m)
          => ConfigLanguage -> m a -> m (a, Text)
commented lang p = do
    indent <- takeWhileP (Just "initial indent") (`elem` (" \t" :: String))
    _ <- chunk $ commentStart (languageComment lang) <> delim
    x <- p
    _ <- chunk (fromMaybe "" $ commentEnd $ languageComment lang)
    space
    eof
    return (x, indent)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[2]
beginBlock :: (MonadParsec e Text m)
           => m ReferenceId
beginBlock = do
    _ <- chunk "begin <<"
    doc  <- cssValue
    _ <- chunk "|"
    name <- cssIdentifier
    _ <- chunk ">>["
    count <- decimal
    _ <- chunk "]"
    _ <- takeRest
    return $ ReferenceId (T.unpack doc) (ReferenceName name) count

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = void $ chunk "end"
-- ~\~ end
-- ~\~ end
