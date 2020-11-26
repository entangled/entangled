-- ~\~ language=Haskell filename=src/Attributes.hs
-- ~\~ begin <<lit/13-tangle.md|src/Attributes.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Attributes where

import RIO
import qualified RIO.Text as T

-- ~\~ begin <<lit/13-tangle.md|attributes-imports>>[0]
import Document (CodeProperty(..))
import Text.Megaparsec
    ( MonadParsec, takeWhileP, chunk, endBy )
import Text.Megaparsec.Char
    ( space, letterChar, char )
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-attributes>>[0]
attributes :: (MonadParsec e Text m)
           => m [CodeProperty]
attributes = (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-attributes>>[1]
cssIdentifier :: (MonadParsec e Text m)
              => m Text
cssIdentifier = do
    firstLetter <- (letterChar <|> char '.')
    rest        <- takeWhileP (Just "identifier")
                        (`notElem` (" {}=<>|" :: String))
    return $ T.singleton firstLetter <> rest

cssValue :: (MonadParsec e Text m)
         => m Text
cssValue = takeWhileP (Just "value")
                      (`notElem` (" {}=<>|" :: String))
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-attributes>>[2]
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = chunk "." >> CodeClass <$> cssIdentifier
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-attributes>>[3]
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = chunk "#" >> CodeId <$> cssIdentifier
-- ~\~ end
-- ~\~ begin <<lit/13-tangle.md|parse-attributes>>[4]
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- cssIdentifier
    _ <- chunk "="
    CodeAttribute key <$> cssValue
-- ~\~ end
-- ~\~ end
