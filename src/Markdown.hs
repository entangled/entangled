{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Markdown 
    ( parseMarkdown
    , parseMarkdown'
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.State

import System.Random
import Data.UUID
import Lens.Micro.Platform

-- import Text.Regex.TDFA
import qualified Text.Parsec as Parsec
import Text.Parsec.Text
import Text.Parsec (
      manyTill, anyChar, try, string, many1, many, noneOf, spaces, endBy, (<|>)
    , eof, endOfLine, lookAhead, newline, option)

import Config
import Model (TangleError, toTangleError)
import Document
import Languages

-- ========================================================================= --
-- Parsers                                                                   --
-- ========================================================================= --

-- data ParserState = ParserState
--     { _psReferenceMap :: ReferenceMap
--     , _psRandomGen :: StdGen
--     }

-- psReferenceMap :: Lens' ParserState ReferenceMap
-- psReferenceMap = lens _psReferenceMap (\s n -> s { _psReferenceMap = n })

-- psRandomGen :: Lens' ParserState StdGen
-- psRandomGen = lens _psRandomGen (\s n -> s { _psRandomGen = n })

type DocumentParser m = Parsec.ParsecT [T.Text] ReferenceMap m
type LineParser = Parsec.Parsec T.Text ()

pCodeClass :: LineParser CodeProperty
pCodeClass = do
    string "."
    cls <- many1 $ noneOf " {}"
    return $ CodeClass cls

pCodeId :: LineParser CodeProperty
pCodeId = do
    string "#"
    id <- many1 $ noneOf " {}"
    return $ CodeId id

pCodeAttribute :: LineParser CodeProperty
pCodeAttribute = do
    tag <- many1 $ noneOf " ={}"
    string "="
    value <- many1 $ noneOf " }"
    return $ CodeAttribute tag value

pCodeProperties :: LineParser [CodeProperty]
pCodeProperties = do
    spaces
    string "{"
    spaces
    props <- (pCodeClass <|> pCodeId <|> pCodeAttribute)
        `endBy` spaces
    string "}"
    return props

pCodeShortClass :: LineParser [CodeProperty]
pCodeShortClass = do
    cls <- many1 $ noneOf " {}"
    return [CodeClass cls]

pCodeDelim :: LineParser [CodeProperty]
pCodeDelim = do
    string "```"
    shortCls <- option [] (try pCodeShortClass)
    longProps <- option [] (try pCodeProperties)
    return $ shortCls ++ longProps

pCodeEnd :: LineParser ()
pCodeEnd = string "```" >> eof

parseDelim :: T.Text -> Maybe (T.Text, [CodeProperty])
parseDelim t 
    = either (const Nothing) (\p -> Just (t, p))
    $ Parsec.runParser pCodeDelim () "" t

parseNormalLine :: T.Text -> Maybe T.Text
parseNormalLine t 
    = either (const $ Just t) (const Nothing)
    $ Parsec.runParser pCodeDelim () "" t

getLanguage :: MonadReader Config m => [CodeProperty] -> m (Maybe Language)
getLanguage [] = return Nothing
getLanguage (CodeClass cls : _) = reader $ lookupLanguage cls
getLanguage (_ : xs) = getLanguage xs

randomNameReference :: (MonadState s m, RandomGen s) => m ReferenceId
randomNameReference = do
    uuid :: UUID <- state random
    return $ NameReferenceId (toString uuid) 0

getReferenceId :: (MonadState s m, RandomGen s) => [CodeProperty] -> DocumentParser m ReferenceId
getReferenceId [] = randomNameReference

getReferenceId (CodeAttribute tag value : xs) =
    case tag of
        "file"    -> return $ FileReferenceId value
        _         -> getReferenceId xs

getReferenceId (CodeId id : _) = do
    n <- countReferences id <$> Parsec.getState
    return $ NameReferenceId id n

getReferenceId (_ : xs) = getReferenceId xs

token :: Monad m => (T.Text -> Maybe a) -> DocumentParser m a
token = Parsec.tokenPrim T.unpack nextPos
    where nextPos p _ _ = Parsec.incSourceLine p 1

fromCodeBlock :: (MonadState s m, RandomGen s) => CodeBlock -> DocumentParser m Content
fromCodeBlock code = do
    id   <- getReferenceId (codeProperties code)
    Parsec.updateState (M.insert id code)
    return $ Reference id

parseCodeBlock' :: (MonadReader Config m, MonadState s m, RandomGen s) => DocumentParser m [Content]
parseCodeBlock' = do
    (begin, props) <- token parseDelim
    content        <- manyTill (token Just) (try $ lookAhead $ token parseDelim)
    (end, _)       <- token parseDelim
    language       <- getLanguage props
    let langName = maybe "<unknown-language>" languageName language
    ref            <- fromCodeBlock $ CodeBlock langName props (unlines' content)
    return [RawText begin, ref, RawText end]

parseNormalBlock' :: Monad m => DocumentParser m [Content]
parseNormalBlock' = do
    text <- unlines' <$> many1 (token parseNormalLine)
    return [RawText text]

parseDocument :: (MonadReader Config m, MonadState s m, RandomGen s)
    => DocumentParser m Document
parseDocument = do
    content <- concat <$> many (try parseCodeBlock' <|> parseNormalBlock')
    refs    <- Parsec.getState
    return $ Document refs content

parseMarkdown' :: (MonadReader Config m, MonadState s m, RandomGen s)
    => ReferenceMap -> FilePath -> T.Text -> m (Either TangleError Document)
parseMarkdown' r f t = toTangleError <$> Parsec.runParserT parseDocument r f (T.lines t)
    
parseMarkdown :: (MonadReader Config m, MonadState s m, RandomGen s)
    => FilePath -> T.Text
    -> m (Either TangleError Document)
parseMarkdown f t = toTangleError 
    <$> Parsec.runParserT parseDocument mempty f (T.lines t)