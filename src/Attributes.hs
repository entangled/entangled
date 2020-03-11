-- ------ language="Haskell" file="src/Attributes.hs" project://lit/13-tangle.md#50
module Attributes where

-- ------ begin <<attributes-imports>>[0] project://lit/13-tangle.md#57
import Data.Text (Text)
import Document (CodeProperty(..))
import Text.Megaparsec
    ( MonadParsec, takeWhile1P, takeWhileP, chunk, endBy, (<|>) )
import Text.Megaparsec.Char
    ( space )
-- ------ end
-- ------ begin <<parse-attributes>>[0] project://lit/13-tangle.md#68
attributes :: (MonadParsec e Text m)
           => m [CodeProperty]
attributes = (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
-- ------ end
-- ------ begin <<parse-attributes>>[1] project://lit/13-tangle.md#81
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = takeWhile1P (Just "identifier")
                            (\c -> notElem c (" {}=<>" :: String))

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (\c -> notElem c (" {}=<>" :: String))
-- ------ end
-- ------ begin <<parse-attributes>>[2] project://lit/13-tangle.md#96
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-attributes>>[3] project://lit/13-tangle.md#107
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-attributes>>[4] project://lit/13-tangle.md#118
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    chunk "="
    value <- cssValue
    return $ CodeAttribute key value
-- ------ end
-- ------ end
