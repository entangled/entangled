-- ------ language="Haskell" file="src/Comment.hs" project://lit/13-tangle.md#434
module Comment where

-- ------ begin <<comment-imports>>[0] project://lit/13-tangle.md#443
import Control.Monad.Reader
import Control.Monad.Except

-- ------ begin <<import-text>>[0] project://lit/01-entangled.md#44
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ end
-- ------ begin <<comment-imports>>[1] project://lit/13-tangle.md#467
import Document
    ( ProgrammingLanguage(..)
    , EntangledError(..) )
import Config
    ( Config(..), ConfigLanguage(..), ConfigComment(..), languageFromName )
-- ------ end
-- ------ begin <<comment-imports>>[2] project://lit/13-tangle.md#496
import qualified Data.Map.Strict as M

import Document
import TextUtil (unlines')
-- ------ end
-- ------ begin <<comment-imports>>[3] project://lit/13-tangle.md#521
import Text.Megaparsec
    ( MonadParsec, chunk, skipManyTill, anySingle, (<?>), takeWhileP, eof )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer
    ( decimal )

import Document (CodeProperty)
import Attributes (attributes, cssIdentifier)
-- ------ end

-- ------ begin <<generate-comment>>[0] project://lit/13-tangle.md#460
delim :: Text
delim = " ~\\~ "
-- ------ end
-- ------ begin <<generate-comment>>[1] project://lit/13-tangle.md#475
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
-- ------ end
-- ------ begin <<generate-comment>>[2] project://lit/13-tangle.md#503
annotateComment :: (MonadReader Config m, MonadError EntangledError m)
                => ReferenceMap -> ReferenceId -> m Text
annotateComment refs ref = do
    let code = refs M.! ref
    pre <- comment (codeLanguage code)
           $ "begin <<" <> (unReferenceName $ referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "]"
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, (codeSource code), post]

headerComment :: ConfigLanguage -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ------ end
-- ------ begin <<parse-comment>>[0] project://lit/13-tangle.md#534
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = do
    skipManyTill (anySingle <?> "open comment")
                 (chunk delim)
    attributes
-- ------ end
-- ------ begin <<parse-comment>>[1] project://lit/13-tangle.md#545
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
-- ------ end
-- ------ begin <<parse-comment>>[2] project://lit/13-tangle.md#558
beginBlock :: (MonadParsec e Text m)
           => m ReferenceId
beginBlock = do
    chunk "begin <<"
    name <- cssIdentifier
    chunk ">>["
    count <- decimal
    chunk "]"
    return $ ReferenceId (ReferenceName name) count

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = chunk "end" >> return ()
-- ------ end
-- ------ end
