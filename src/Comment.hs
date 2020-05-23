-- ~\~ language=Haskell filename=src/Comment.hs
-- ~\~ begin <<lit/13-tangle.md|src/Comment.hs>>[0]
module Comment where

-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[0]
import Control.Monad.Reader
import Control.Monad.Except

-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ~\~ end
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[1]
import Document
    ( ProgrammingLanguage(..)
    , EntangledError(..) )
import Config
    ( Config(..), ConfigLanguage(..), ConfigComment(..), languageFromName )
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[2]
import qualified Data.Map.Strict as M

import Document
import TextUtil (unlines')
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|comment-imports>>[3]
import Text.Megaparsec
    ( MonadParsec, chunk, skipManyTill, anySingle, (<?>), takeWhileP, eof )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer
    ( decimal )

import Document (CodeProperty)
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
comment NoLanguage         _ = throwError $ MissingLanguageClass
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
    where pre  = (commentStart $ languageComment lang) <> delim
          post = maybe "" (" " <>) $ commentEnd $ languageComment lang
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|generate-comment>>[2]
annotateComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> ReferenceId -> m Text
annotateComment refs ref = do
    let code = refs M.! ref
    pre <- comment (codeLanguage code)
           $ "begin <<" <> (T.pack $ referenceFile ref) <> "|"
           <> (unReferenceName $ referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "]"
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, (codeSource code), post]

headerComment :: ConfigLanguage -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[0]
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = do
    skipManyTill (anySingle <?> "open comment")
                 (chunk delim)
    attributes
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[1]
commented :: (MonadParsec e Text m)
          => ConfigLanguage -> m a -> m (a, Text)
commented lang p = do
    indent <- takeWhileP (Just "initial indent") (`elem` (" \t" :: [Char]))
    pre <- chunk $ (commentStart $ languageComment lang) <> delim
    x <- p
    post <- chunk $ (maybe "" id $ commentEnd $ languageComment lang)
    space
    eof
    return (x, indent)
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-comment>>[2]
beginBlock :: (MonadParsec e Text m)
           => m ReferenceId
beginBlock = do
    chunk "begin <<"
    doc  <- cssValue
    chunk "|"
    name <- cssIdentifier
    chunk ">>["
    count <- decimal
    chunk "]"
    return $ ReferenceId (T.unpack doc) (ReferenceName name) count

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = chunk "end" >> return ()
-- ~\~ end
-- ~\~ end
