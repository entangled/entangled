module Untangle
    ( untangle
    , untangleSpec
    ) where

import Config
import Model (TangleError, toTangleError)
import Document
import Languages

import Control.Monad.Reader
import Control.Monad (when)

import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Data.Text as T

import Data.Char(isSpace)
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

import Debug.Trace

data Header = Header
    { headerFilename :: String
    , headerLanguage :: String
    } deriving (Eq, Show)

data ReferenceTag = ReferenceTag
    { rtName   :: String
    , rtIndex  :: Int
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
    { psLanguage :: Language
    , psRefs     :: ReferenceMap
    } | EmptyState deriving (Show, Eq)

updateRefs :: (ReferenceMap -> ReferenceMap) -> ParserState -> ParserState
updateRefs f s@(ParserState _ r) = s { psRefs = f r}

getRefs :: Monad m => Parser m ReferenceMap
getRefs = psRefs <$> Parsec.getState

getLanguage :: Monad m => Parser m Language
getLanguage = psLanguage <$> Parsec.getState

getComment :: Monad m => Parser m String
getComment = languageLineComment <$> getLanguage

addReference :: Monad m => ReferenceId -> CodeBlock -> Parser m ()
addReference r c = Parsec.modifyState $ updateRefs $ Map.insert r c

-- ========================================================================= --
-- Parser type definition and fundamentals                                   --
-- ========================================================================= --

type Parser m = Parsec.ParsecT [String] ParserState (ReaderT Config m)

parse :: Monad m => Parser m a -> String -> [String] -> ReaderT Config m (Either Parsec.ParseError a)
parse p = Parsec.runParserT p EmptyState

token :: Monad m => (String -> Maybe a) -> Parser m a
token = Parsec.tokenPrim id nextPos
    where nextPos p _ _ = Parsec.incSourceLine p 1

-- Set language from name and error if cannot find it
setLanguage languageName =
  case languageFromName languageName defaultConfig of
    Nothing -> fail $ "Unknown language: " ++ languageName
    Just c  -> Parsec.setState $ ParserState c mempty

-- ========================================================================= --
-- Parsers                                                                   --
-- ========================================================================= --

document :: Monad m => Parser m ReferenceMap
document = do
    header <- token matchHeader
    pos <- getPosition
    let language = headerLanguage header
    setLanguage language

    comment <- getComment
    content <- intercalate "\n" . catMaybes
        <$> manyTill (reference <|> Just <$> anyToken)
                     (token $ matchEnd comment)

    addReference (FileReferenceId $ headerFilename header)
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

    ref     <- try $ token $ matchReference     comment
    inp <- Parsec.getInput
    trace ("BEFORE: " <> show inp) $ return ()
    _       <- try $ token $ matchLineDirective language
    trace ("AFTER:  " <> show inp) $ return ()
    pos     <- getPosition
    lines   <- catMaybes <$> manyTill (reference <|> Just <$> anyToken)
                                      (token $ matchEnd comment)
    let strippedContent = map (unindent $ rtIndent ref) lines
    when (isJust $ find isNothing strippedContent) $ fail "Indentation error"
    let content = intercalate "\n" $ catMaybes strippedContent
    trace ("CONTENT: " <> content) $ return ()

    addReference (NameReferenceId (rtName ref) (rtIndex ref))
                 (CodeBlock (languageName language) [] (T.pack content) pos)

    if rtIndex ref == 0
        then return $ Just $ rtIndent ref ++ "<<" ++ rtName ref ++ ">>"
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

matchLineDirective          :: Language -> String -> Maybe ()
matchLineDirective lang line =
 case checkForLineDirective (languageLineDirective lang) $ T.pack $ dropWhile isSpace line of
   True  -> Just ()
   False -> Nothing

referencePattern :: String -> String
referencePattern comment = "^([ \\t]*)" ++ escape comment
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
            matchReference "//" "    // begin <<hello-world>>[1]" `shouldBe`
                (Just $ ReferenceTag "hello-world" 1 "    ")
        it "ignores fails" $
            matchReference "//" "std::cout << \"// <<this is actual code>>[9]\";" `shouldSatisfy`
                isNothing
    describe "Line directive" $ do
        it "match line directive" $ do
            let Just cpp = languageFromName "C++" defaultConfig
            matchLineDirective cpp "#LINE 17 \"hello.c\"" `shouldSatisfy`
                isJust
        it "match indented line directive" $ do
            let Just cpp = languageFromName "C++" defaultConfig
            matchLineDirective cpp "     #LINE 17 \"hello.c\"" `shouldSatisfy`
                isJust
        it "ignore wrong directive" $ do
            let Just cpp = languageFromName "C++" defaultConfig
            matchLineDirective cpp "{-# LINE 17 \"hello.hs\" #-}" `shouldSatisfy`
                isNothing

    describe "Untangle.matchEnd" $ do
        it "matches an end line" $
            matchEnd "//" "// end" `shouldSatisfy` isJust
        it "doesn't match something else" $
            matchEnd "//" "something else" `shouldSatisfy` isNothing

