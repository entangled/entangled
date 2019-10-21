-- ------ language="Haskell" file="src/Comment.hs"
module Comment where

-- ------ begin <<comment-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import qualified Data.Map.Strict as M

import Document
    ( CodeBlock(..)
    , Document(..)
    , EntangledError(..)
    , ProgrammingLanguage(..)
    , ReferenceId(..)
    , ReferenceName(..)
    , unlines'
    )

import Config
    ( Language(..)
    )
-- ------ end

-- ------ begin <<generate-comment>>[0]
comment :: ProgrammingLanguage
        -> Text
        -> Either EntangledError Text
comment (UnknownClass cls) _ = Left $ UnknownLanguageClass cls
comment NoLanguage         _ = Left $ MissingLanguageClass
comment (KnownLanguage lang) text = Right $ formatComment lang text

formatComment :: Language -> Text -> Text
formatComment lang text = pre <> text <> post
    where pre  = languageStartComment lang <> "###"
          post = "###" <> (maybe "" id $ languageCloseComment lang)

annotateComment :: Document -> ReferenceId -> Either EntangledError Text
annotateComment doc ref = do
    let code = references doc M.! ref
    pre <- comment (codeLanguage code)
           $ " begin <<" <> (unReferenceName $ referenceName ref) <> ">>["
           <> T.pack (show $ referenceCount ref) <> "] "
    post <- comment (codeLanguage code) " end "
    return $ unlines' [pre, (codeSource code), post]

headerComment :: Language -> FilePath -> Text
headerComment lang path = formatComment lang
    $ "language=" <> languageName lang <> " filename=" <> T.pack path
-- ------ end
-- ------ begin <<parse-comment>>[0]

-- ------ end
-- ------ end
