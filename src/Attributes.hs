-- ------ language="Haskell" file="src/Attributes.hs"
module Attributes where

-- ------ begin <<attributes-imports>>[0]
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import Document (CodeProperty(..))
import Text.Megaparsec
    ( MonadParsec, takeWhile1P, takeWhileP, chunk, endBy, (<|>) )
import Text.Megaparsec.Char
    ( space )
-- ------ end
-- ------ begin <<parse-attributes>>[0]
attributes :: (MonadParsec e Text m)
           => m [CodeProperty]
attributes = (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
-- ------ end
-- ------ begin <<parse-attributes>>[1]
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = takeWhile1P (Just "identifier")
                            (\c -> notElem c (" {}=<>" :: String))

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (\c -> notElem c (" {}=<>" :: String))
-- ------ end
-- ------ begin <<parse-attributes>>[2]
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-attributes>>[3]
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> cssIdentifier
-- ------ end
-- ------ begin <<parse-attributes>>[4]
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    chunk "="
    value <- cssValue
    return $ CodeAttribute key value
-- ------ end
-- ------ end
