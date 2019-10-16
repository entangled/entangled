-- ------ language="Haskell" file="app/Tangle.hs"
{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Tangle where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-megaparsec>>[0]
import Text.Megaparsec
    ( MonadParsec, Parsec, parse
    , noneOf, chunk, many, some, endBy, eof, token
    , manyTill, anySingle, try, lookAhead
    , (<|>), (<?>) )
import Text.Megaparsec.Char
    ( space )
import Data.Void
-- ------ end

import Control.Monad.Reader (MonadReader)

import ListStream (ListStream (..))
import Document
    ( CodeProperty (..), CodeBlock (..), Content (..), ReferenceId (..)
    , ReferencePair, ProgrammingLanguage (..), unlines')
import Config (Config, languageName)

-- ------ begin <<parse-markdown>>[0]

-- ------ end
-- ------ begin <<parse-markdown>>[1]
codeHeader :: (MonadParsec e Text m)
           => m [CodeProperty]
codeHeader = do
    chunk "```" >> space >> chunk "{" >> space
    props <- (  codeClass
            <|> codeId
            <|> codeAttribute
             ) `endBy` space
    chunk "}" >> space >> eof
    return props 

codeFooter :: (MonadParsec e Text m)
           => m ()
codeFooter = chunk "```" >> space >> eof
-- ------ end
-- ------ begin <<parse-markdown>>[2]
type LineParser = Parsec Void Text
-- data DocumentParserT m = ParsecT Void [Text] (StateT ReferenceMap m)

parseLine :: LineParser a -> Text -> Maybe (a, Text)
parseLine p t = either (const Nothing) (\x -> Just (x, t))
              $ parse p "" t

parseLineNot :: (Monoid a) => LineParser a -> Text -> Maybe Text
parseLineNot p t = either (const $ Just t) (const Nothing)
                 $ parse p "" t

tokenLine :: ( MonadParsec e (ListStream Text) m )
          => (Text -> Maybe a) -> m a
tokenLine f = token f mempty

codeBlock :: ( MonadParsec e (ListStream Text) m,
               MonadReader Config m )
          => m ([Content], [ReferencePair])
codeBlock = do
    (begin, props) <- tokenLine (parseLine codeHeader)
    code           <- unlines' <$> manyTill (anySingle <?> "code line")
                                            (try $ lookAhead $ tokenLine (parseLine codeFooter))
    (end, _)       <- tokenLine (parseLine codeFooter)
    language       <- getLanguage props
    return $ case getReference props of
        Nothing  -> ( [ PlainText $ unlines' [begin, code, end] ], [] )
        Just ref -> ( [ PlainText begin, ref, PlainText end ]
                    , [ ( ref, CodeBlock language props code ) ] )

normalText :: ( MonadParsec e (ListStream Text) m )
           => m ([Content], [ReferencePair])
normalText = do
    text <- unlines' <$> some (tokenLine (parseLineNot codeHeader))
    return ( [ PlainText text ], [] )
-- ------ end
-- ------ begin <<parse-markdown>>[3]
getLanguage :: ( MonadReader Config m )
            => [CodeProperty] -> m ProgrammingLanguage
getLanguage [] = return NoLanguage
getLanguage (CodeClass cls : _)
    = maybe (UnknownLanguage cls) 
            (KnownLanguage . languageName)
            <$> reader (lookupLanguage cls)
getLanguage (_ : xs) = getLanguage xs

getReference :: [CodeProperty] -> Maybe ReferenceId
getReference [] = Nothing
getReference (CodeId x:_) = Just $ NameReferenceId x 0
getReference (CodeAttribute k v:xs)
    | k == "file" = Just $ FileReferenceId v
    | otherwise   = getReference xs
getReference (_:xs) = getReference xs
-- ------ end
-- ------ begin <<parse-markdown>>[4]
codeClass :: (MonadParsec e Text m)
          => m CodeProperty
codeClass = do
    chunk "."
    CodeClass <$> some (noneOf " {}")
-- ------ end
-- ------ begin <<parse-markdown>>[5]
codeId :: (MonadParsec e Text m)
       => m CodeProperty
codeId = do
    chunk "#"
    CodeId <$> some (noneOf " {}")
-- ------ end
-- ------ begin <<parse-markdown>>[6]
codeAttribute :: (MonadParsec e Text m)
              => m CodeProperty
codeAttribute = do
    key <- some (noneOf " ={}")
    chunk "="
    value <- some (noneOf " {}")
    return $ CodeAttribute key value
-- ------ end
-- ------ end
