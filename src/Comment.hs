-- ~\~ language=Haskell filename=src/Comment.hs
-- ~\~ begin <<lit/13-tangle.md|src/Comment.hs>>[init]
{-# LANGUAGE NoImplicitPrelude,TupleSections #-}
module Comment where

import RIO
import qualified RIO.Text as T

-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[init]
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
import qualified Format
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[3]
import Text.Megaparsec            ( MonadParsec, chunk, skipManyTill
                                  , anySingle, (<?>), takeWhileP, takeRest )
import Text.Megaparsec.Char.Lexer ( decimal )

import Attributes (attributes, cssIdentifier, cssValue)
-- ~\~ end

-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[init]
delim :: Text
delim = " ~\\~ "
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[1]
getLangName :: (MonadError EntangledError m)
            => ProgrammingLanguage -> m Text
getLangName (UnknownClass cls)       = throwError $ UnknownLanguageClass cls
getLangName NoLanguage               = throwError MissingLanguageClass
getLangName (KnownLanguage langName) = return langName

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
standardPreComment :: Bool -> ReferenceId -> Text
standardPreComment init (ReferenceId file (ReferenceName name) count) =
    "begin <<" <> T.pack file <> "|" <> name <> ">>[" <> counter <> "]"
    where counter = if init then "init" else tshow count

getReference :: (MonadError EntangledError m) => ReferenceMap -> ReferenceId -> m CodeBlock
getReference refs ref = maybe (throwError $ ReferenceError $ "not found: " <> tshow ref)
                              return (refs M.!? ref)

lineDirective :: (MonadReader Config m, MonadError EntangledError m)
              => ReferenceId -> CodeBlock -> m Text
lineDirective ref code = do
    Config{..} <- ask
    lang <- getLangName $ codeLanguage code
    spec <- maybe (throwError $ ConfigError $ "line directives not configured for " <> lang)
                  return (configLineDirectives M.!? lang)
    maybe (throwError $ ConfigError $ "error formatting on " <> tshow spec)
          return (Format.formatMaybe spec $ M.fromList [ ("linenumber" :: Text, tshow (fromMaybe 0 $ codeLineNumber code))
                                                       , ("filename"          , T.pack (referenceFile ref))])

annotateNaked :: (MonadReader Config m, MonadError EntangledError m)
              => ReferenceMap -> Bool -> ReferenceId -> m Text
annotateNaked refs _ ref = do
    Config{..} <- ask
    code <- getReference refs ref
    if configUseLineDirectives then do
        line <- lineDirective ref code
        return $ unlines' [line, codeSource code]
    else return $ codeSource code

annotateComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> Bool -> ReferenceId -> m Text
annotateComment refs init ref = do
    code <- getReference refs ref
    naked <- annotateNaked refs init ref
    pre <- comment (codeLanguage code) $ standardPreComment init ref
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, naked, post]

annotateProject :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> Bool -> ReferenceId -> m Text
annotateProject refs init ref@(ReferenceId file _ _) = do
    code <- getReference refs ref
    naked <- annotateNaked refs init ref
    let line = fromMaybe 0 (codeLineNumber code)
    pre  <- comment (codeLanguage code) (standardPreComment init ref <> " project://" <> T.pack file <> "#" <> tshow line)
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, naked, post]

headerComment :: ConfigLanguage -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[init]
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = do
    _    <- skipManyTill (anySingle <?> "open comment")
                         (chunk delim)
    attr <- attributes
    _    <- takeRest
    return attr
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[1]
commented :: (MonadParsec e Text m)
          => ConfigLanguage -> m a -> m (a, Text)
commented lang p = do
    indent <- takeWhileP (Just "initial indent") (`elem` (" \t" :: String))
    _ <- chunk $ commentStart (languageComment lang) <> delim
    x <- p
    -- _ <- chunk (fromMaybe "" $ commentEnd $ languageComment lang)
    -- space
    -- eof
    _ <- takeRest
    return (x, indent)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[2]
countOrInit :: (MonadParsec e Text m) => m (Int, Bool)
countOrInit = ((0, True) <$ chunk "init") <|> ((, False) <$> decimal)

beginBlock :: (MonadParsec e Text m)
           => m (ReferenceId, Bool)
beginBlock = do
    _ <- chunk "begin <<"
    doc  <- cssValue
    _ <- chunk "|"
    name <- cssIdentifier
    _ <- chunk ">>["
    (count, init) <- countOrInit
    _ <- chunk "]"
    _ <- takeRest
    return (ReferenceId (T.unpack doc) (ReferenceName name) count, init)

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = void $ chunk "end"
-- ~\~ end
-- ~\~ end
