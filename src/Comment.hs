-- ------ language="Haskell" file="src/Comment.hs"
module Comment where

-- ------ begin <<comment-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ end
-- ------ begin <<comment-imports>>[1]
import Document
    ( ProgrammingLanguage(..)
    , EntangledError(..) )
import Config
    ( Language(..) )
-- ------ end
-- ------ begin <<comment-imports>>[2]
import qualified Data.Map.Strict as M

import Document
    ( CodeBlock(..)
    , Document(..)
    , ReferenceId(..)
    , ReferenceName(..)
    )
import TextUtil (unlines')
-- ------ end
-- ------ begin <<comment-imports>>[3]
import Text.Megaparsec
    ( MonadParsec, chunk, skipManyTill, anySingle, (<?>), takeWhileP, eof )
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer
    ( decimal )

import Document (CodeProperty)
import Attributes (attributes, cssIdentifier)
-- ------ end

-- ------ begin <<generate-comment>>[0]
delim :: Text
delim = "-#- entangled -#- "
-- ------ end
-- ------ begin <<generate-comment>>[1]
comment :: ProgrammingLanguage
        -> Text
        -> Either EntangledError Text
comment (UnknownClass cls) _ = Left $ UnknownLanguageClass cls
comment NoLanguage         _ = Left $ MissingLanguageClass
comment (KnownLanguage lang) text = Right $ formatComment lang text

formatComment :: Language -> Text -> Text
formatComment lang text = pre <> text <> post
    where pre  = languageStartComment lang <> delim
          post = maybe "" id $ languageCloseComment lang
-- ------ end
-- ------ begin <<generate-comment>>[2]
annotateComment :: Document -> ReferenceId -> Either EntangledError Text
annotateComment doc ref = do
    let code = references doc M.! ref
    pre <- comment (codeLanguage code)
           $ "begin <<" <> (unReferenceName $ referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "]"
    post <- comment (codeLanguage code) "end"
    return $ unlines' [pre, (codeSource code), post]

headerComment :: Language -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ------ end
-- ------ begin <<parse-comment>>[0]
topHeader :: ( MonadParsec e Text m )
          => m [CodeProperty]
topHeader = do
    skipManyTill (anySingle <?> "open comment")
                 (chunk delim)
    attributes
-- ------ end
-- ------ begin <<parse-comment>>[1]
commented :: (MonadParsec e Text m)
          => Language -> m a -> m (a, Text)
commented lang p = do 
    indent <- takeWhileP Nothing (`elem` (" \t" :: [Char]))
    pre <- chunk $ (languageStartComment lang) <> delim
    x <- p
    post <- chunk $ (maybe "" id $ languageCloseComment lang)
    space
    eof
    return (x, indent)
-- ------ end
-- ------ begin <<parse-comment>>[2]
beginBlock :: (MonadParsec e Text m)
           => m ReferenceId
beginBlock = do
    chunk " begin <<"
    name <- cssIdentifier
    chunk ">>["
    count <- decimal
    chunk "]"
    return $ ReferenceId (ReferenceName name) count

endBlock :: (MonadParsec e Text m)
         => m ()
endBlock = chunk " end " >> return ()
-- ------ end
-- ------ end
