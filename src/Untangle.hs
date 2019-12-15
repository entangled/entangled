module Untangle
    ( untangle
    , untangleSpec
    ) where

import Config
import Model (TangleError, toTangleError)
import Document

import Control.Monad.Reader
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Data.Text as T

import Data.Maybe
import Data.String
import Data.List

import Text.Regex.TDFA
import Text.Read
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), try, many, anyToken, manyTill, getPosition)

import Test.Hspec
import Data.Either.Combinators (fromRight')
import Data.Either

data Header = Header
    { headerFilename :: String
    , headerLanguage :: String
    } deriving (Eq, Show)

data ReferenceTag = ReferenceTag
    { rtName   :: String
    , rtIndex  :: Int
    , rtSource :: String
    , rtIndent :: String
    } deriving (Eq, Show)

-- ========================================================================= --
-- Parser state access functions                                             --
-- ========================================================================= --

{-| State type for the line parser that extracts code blocks from
    annotated tangled source files.

    The parser builds a 'ReferenceMap', each time updating the state.
    Each generated code block need to know its language. We also
    store the comment string here.
 -}
data ParserState = ParserState
    { psComment :: String
    , psLanguage :: String
    , psRefs :: ReferenceMap
    , psLastRef :: ReferenceId
    } | EmptyState deriving (Show, Eq)

updateRefs :: ReferenceId -> (ReferenceMap -> ReferenceMap) -> ParserState -> ParserState
updateRefs newRef f s@(ParserState _ _ r _) = s { psRefs = f r, psLastRef = newRef }

getRefs :: Monad m => Parser m ReferenceMap
getRefs = psRefs <$> Parsec.getState

getLastRef :: Monad m => Parser m ReferenceId
getLastRef = psLastRef <$> Parsec.getState

getLanguage :: Monad m => Parser m String
getLanguage = psLanguage <$> Parsec.getState

getComment :: Monad m => Parser m String
getComment = psComment <$> Parsec.getState

addReference :: Monad m => ReferenceId -> CodeBlock -> Parser m ()
addReference r c = Parsec.modifyState $ updateRefs r $ Map.insert r c

-- ========================================================================= --
-- Parser type definition and fundamentals                                   --
-- ========================================================================= --

type Parser m = Parsec.ParsecT [String] ParserState (ReaderT Config m)

parse :: Monad m => Parser m a -> String -> [String] -> ReaderT Config m (Either Parsec.ParseError a)
parse p = Parsec.runParserT p EmptyState

token :: Monad m => (String -> Maybe a) -> Parser m a
token = Parsec.tokenPrim id nextPos
    where nextPos p _ _ = Parsec.incSourceLine p 1

-- ========================================================================= --
-- Parsers                                                                   --
-- ========================================================================= --

document :: Monad m => Parser m ReferenceMap
document = do
    header <- token matchHeader
    pos <- getPosition
    let language = headerLanguage header
    let fileRef = FileReferenceId $ headerFilename header
    commentString <- asks $ getCommentString language
    case commentString of
        Nothing -> fail $ "Unknown language: " ++ language
        Just c  -> Parsec.setState $ ParserState c language mempty fileRef

    comment <- getComment
    content <- intercalate "\n" . catMaybes
        <$> manyTill (reference <|> Just <$> anyToken)
                     (token $ matchEnd comment)

    addReference fileRef
                 (CodeBlock language [] (T.pack content) pos)
    getRefs

unindent :: String -> String -> Maybe String
unindent prefix s =
    if s =~ "^[ \\t]*$"
    then Just ""
    else stripPrefix prefix s

reference :: Monad m => Parser m (Maybe String)
reference = do
    language <- getLanguage
    comment  <- getComment
    lastRef  <- getLastRef

    ref     <- try $ token $ matchReference comment
    let name = (rtName ref)
    pos     <- getPosition
    lines   <- catMaybes <$> manyTill (reference <|> Just <$> anyToken)
                                      (token $ matchEnd comment)
    let strippedContent = map (unindent $ rtIndent ref) lines
    when (isJust $ find isNothing strippedContent) $ fail "Indentation error"
    let content = intercalate "\n" $ catMaybes strippedContent

    addReference (NameReferenceId name (rtSource ref) (rtIndex ref))
                 (CodeBlock language [] (T.pack content) pos)

    if rtIndex ref == 0 && name /= referenceName lastRef
        then return $ Just $ rtIndent ref ++ "<<" ++ name ++ ">>"
        else return Nothing

-- ========================================================================= --
-- Regular expressions for matching lines                                    --
-- ========================================================================= --

escape :: String -> String
escape = concatMap escapeChar
    where escapeChar c
            | c `elem` ".*()[]" = ['\\', c]
            | otherwise = [c]

headerPattern :: String
headerPattern = "^.+language=\"([^\"].+)\"[ \\t]+file=\"([^\"]+)\""

matchHeader :: String -> Maybe Header
matchHeader line =
    case line =~ headerPattern :: [MatchText String] of
        []  -> Nothing
        [m] -> Just $ Header filename language
            where (filename, _) = m Array.! 2
                  (language, _) = m Array.! 1

referencePattern :: String -> String
referencePattern comment = "^([ \\t]*)" ++ escape comment
    ++ "[ \\t]+begin[ \\t]+<<(.+)>>\\[([0-9A-Za-z]+):([0-9]+)\\]"

matchReference :: String -> String -> Maybe ReferenceTag
matchReference comment line =
    case line =~ referencePattern comment :: [MatchText String] of
        []  -> Nothing
        [m] -> do
            let (name, _)        = m Array.! 2
                (indexString, _) = m Array.! 4
                (source, _)      = m Array.! 3
                (indent, _)      = m Array.! 1
            idx <- readMaybe indexString :: Maybe Int
            return $ ReferenceTag name idx source indent

endPattern :: String -> String
endPattern comment = "^[ \\t]*" ++ escape comment
    ++ "[ \\t]+end"

matchEnd :: String -> String -> Maybe ()
matchEnd comment line =
    if line =~ endPattern comment :: Bool
        then Just ()
        else Nothing

-- ========================================================================= --
-- The one important function                                                --
-- ========================================================================= --

untangle :: Monad m => String -> String -> ReaderT Config m (Either TangleError ReferenceMap)
untangle filename text = toTangleError <$> parse document filename (lines text)

-- ========================================================================= --
-- Unit tests                                                                --
-- ========================================================================= --

untangleSpec :: Spec
untangleSpec = do
    describe "Untangle.matchHeader" $ do
        it "unpacks a header line" $
            matchHeader "// language=\"C++\" file=\"hello.cc\"" `shouldBe`
                (Just $ Header "hello.cc" "C++")
        it "but fails when it needs to" $
            matchHeader "// This is an ordinary comment on files and languages" `shouldSatisfy`
                isNothing

    describe "Untangle.matchReference" $ do
        it "unpacks a reference line" $
            matchReference "//" "    // begin <<hello-world>>[src:1]" `shouldBe`
                (Just $ ReferenceTag "hello-world" 1 "src" "    ")
        it "ignores fails" $
            matchReference "//" "std::cout << \"// <<this is actual code>>[src:9]\";" `shouldSatisfy`
                isNothing

    describe "Untangle.matchEnd" $ do
        it "matches an end line" $
            matchEnd "//" "// end" `shouldSatisfy` isJust
        it "doesn't match something else" $
            matchEnd "//" "something else" `shouldSatisfy` isNothing

