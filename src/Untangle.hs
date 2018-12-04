module Untangle
    ( untangle
    , Header(..)
    , ReferenceTag(..)
    , matchHeader
    , matchReference
    , matchEnd
    ) where

import Model
import Config

import Control.Monad.Reader
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Maybe
import Data.String
import Data.List

import Text.Regex.TDFA
import Text.Read
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), try, many, anyToken, manyTill)

data Header = Header
    { headerFilename :: String
    , headerLanguage :: String
    } deriving (Eq, Show)

data ReferenceTag = ReferenceTag
    { rtName   :: String
    , rtIndex  :: Int
    , rtIndent :: String
    } deriving (Eq, Show)

data ParserState = ParserState
    { psComment :: String
    , psLanguage :: String
    , psRefs :: ReferenceMap
    } | EmptyState deriving (Show, Eq)

updateRefs :: (ReferenceMap -> ReferenceMap) -> ParserState -> ParserState
updateRefs f s@(ParserState _ _ r) = s { psRefs = f r}

getRefs :: Parser ReferenceMap
getRefs = psRefs <$> Parsec.getState

getLanguage :: Parser String
getLanguage = psLanguage <$> Parsec.getState

getComment :: Parser String
getComment = psComment <$> Parsec.getState

addReference :: ReferenceID -> CodeBlock -> Parser ()
addReference r c = Parsec.modifyState $ updateRefs $ Map.insert r c

type Parser = Parsec.ParsecT [String] ParserState (Reader Config)

parse :: Parser a -> String -> [String] -> Reader Config (Either Parsec.ParseError a)
parse p = Parsec.runParserT p EmptyState

token :: (String -> Maybe a) -> Parser a
token = Parsec.tokenPrim id nextPos
    where nextPos p _ _ = Parsec.incSourceLine p 1

document :: Parser ReferenceMap
document = do
    header <- token matchHeader
    let language = headerLanguage header
    commentString <- asks $ getCommentString language
    case commentString of
        Nothing -> fail $ "Unknown language: " ++ language
        Just c  -> Parsec.setState $ ParserState c language emptyReferenceMap

    comment <- getComment
    content <- intercalate "\n" . catMaybes
        <$> manyTill (try reference <|> Just <$> anyToken)
                     (token $ matchEnd comment)

    addReference (FileReferenceID $ headerFilename header)
                 (CodeBlock language content)
    getRefs

reference :: Parser (Maybe String)
reference = do
    language <- getLanguage
    comment  <- getComment

    ref     <- token $ matchReference comment
    lines   <- catMaybes <$> manyTill (try reference <|> Just <$> anyToken)
                                      (token $ matchEnd comment)
    let strippedContent = map (stripPrefix $ rtIndent ref) lines
    when (isJust $ find isNothing strippedContent) $ fail "Indentation error"
    let content = intercalate "\n" $ catMaybes strippedContent

    addReference (NameReferenceID (rtName ref) (rtIndex ref))
                 (CodeBlock language content)

    if rtIndex ref == 0
        then return $ Just $ rtIndent ref ++ "<<" ++ rtName ref ++ ">>"
        else return Nothing

headerPattern :: String
headerPattern = "^[^ \\t]+[ \\t]+language=\"([^\"].+)\"[ \\t]+file=\"([^\"]+)\"[ \\t]*$"

matchHeader :: String -> Maybe Header
matchHeader line =
    case line =~ headerPattern :: [MatchText String] of
        []  -> Nothing
        [m] -> Just $ Header filename language
            where (filename, _) = m Array.! 2
                  (language, _) = m Array.! 1

referencePattern :: String -> String
referencePattern comment = "^([ \\t]*)" ++ comment
    ++ "[ \\t]+begin[ \\t]+<<(.+)>>\\[([0-9]+)\\]"
                                    
matchReference :: String -> String -> Maybe ReferenceTag
matchReference comment line =
    case line =~ referencePattern comment :: [MatchText String] of
        []  -> Nothing
        [m] -> do
            let (name, _)        = m Array.! 2
                (indexString, _) = m Array.! 3
                (indent, _)      = m Array.! 1
            idx <- readMaybe indexString :: Maybe Int
            return $ ReferenceTag name idx indent

endPattern :: String -> String
endPattern comment = "^[ \\t]*" ++ comment
    ++ "[ \\t]+end[ \\t]*$"

matchEnd :: String -> String -> Maybe ()
matchEnd comment line = 
    if line =~ endPattern comment :: Bool
        then Just ()
        else Nothing

untangle :: String -> String -> Reader Config (Either Parsec.ParseError ReferenceMap)
untangle filename text = parse document filename (lines text)
