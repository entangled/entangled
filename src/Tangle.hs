module Tangle
    ( tangleNaked
    , tangleAnnotated
    , tangleSpec
    ) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader
import Data.Functor.Identity (Identity(..))
import qualified Data.Text as T

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), manyTill, anyChar, try, endOfLine, many, oneOf, noneOf, string, many1)

import Model
import Config
import Languages

import Test.Hspec
import Data.Either.Combinators (fromRight')
import Data.Either

type FileMap = Map.Map FilePath (Either TangleError T.Text)

data Source = SourceText String 
            | NowebReference String String
            deriving (Eq, Show)

type Parser = Parsec.ParsecT String () Identity

parse :: Parser a -> String -> String -> Either Parsec.ParseError a
parse p n t = x
    where Identity x = Parsec.runParserT p () n t

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorval Nothing = Left errorval
maybeToEither _ (Just normalval) = Right normalval

nowebReference :: Parser Source
nowebReference = do
    indent <- many (oneOf " \t")
    string "<<"
    id <- many1 $ noneOf " \t<>"
    string ">>"
    many (oneOf " \t")
    endOfLine
    return $ NowebReference id indent

skip :: Parser c -> Parser ()
skip a = do
    x <- a
    return ()

line :: Parser String
line = manyTill anyChar (try endOfLine)

codeLine :: Parser Source
codeLine = SourceText <$> line

nowebCode :: Parser [Source]
nowebCode = many (try nowebReference <|> codeLine)

parseCode :: ReferenceID -> String -> Either TangleError [Source]
parseCode id x = case parse nowebCode (referenceName id) x of
    Left  err -> Left $ TangleError $ show err
    Right src -> Right src

addIndent :: String -> T.Text -> T.Text
addIndent indent text 
    = T.intercalate (T.pack "\n") 
    $ map (T.append (T.pack indent)) $ T.lines text

type Expander = ReferenceID -> Either TangleError T.Text

codeLineToString :: ReferenceMap -> Expander -> Source -> Either TangleError T.Text
codeLineToString _ _ (SourceText x) = Right $ T.pack x
codeLineToString refs e (NowebReference r i) = 
    addIndent i . T.concat <$> mapM e (findAllNamedReferences r refs)

codeListToString :: ReferenceMap -> Expander -> [Source] -> Either TangleError T.Text
codeListToString refs e srcs = T.unlines <$> mapM (codeLineToString refs e) srcs

expandGeneric :: Expander -> (CodeBlock -> Either TangleError T.Text) -> ReferenceMap
              -> ReferenceID -> Either TangleError T.Text
expandGeneric e codeToText refs id = do
    codeBlock <- maybeToEither 
        (TangleError $ "Reference " ++ show id ++ " not found.")
        (refs Map.!? id)
    text <- codeToText codeBlock
    parsedCode <- parseCode id $ T.unpack text ++ "\n"
    codeListToString refs e parsedCode

expandNaked :: ReferenceMap -> ReferenceID -> Either TangleError T.Text
expandNaked refs = expandGeneric (expandNaked refs) (Right . T.pack . codeSource) refs

tangleNaked :: Document -> FileMap
tangleNaked (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandNaked refs) fileRefs
          fileNames = map referenceName fileRefs

{- Annotated tangle -}
topAnnotation :: ReferenceID -> String -> String -> String
topAnnotation (NameReferenceID n i) comment lang =
    comment ++ " begin <<" ++ n ++ ">>[" ++ show i ++ "]"
topAnnotation (FileReferenceID n) comment lang =
    comment ++ " language=\"" ++ lang ++ "\" file=\"" ++ n ++ "\""

lookupLanguage' :: Monad m => String -> ReaderT Config  m (Either TangleError Language)
lookupLanguage' name = do
    lang <- reader $ languageFromName name
    case lang of
        Nothing -> return $ Left $ TangleError $ "unknown language: " ++ name
        Just l  -> return $ Right l

annotate :: Monad m => ReferenceID -> CodeBlock -> ReaderT Config m (Either TangleError T.Text)
annotate id (CodeBlock lang text) = do
    l <- lookupLanguage' lang
    return $ do 
        (Language _ _ comment) <- l
        let top      = topAnnotation id comment lang
            bottom   = comment ++ " end"
        return $ T.pack $ top ++ "\n" ++ text ++ "\n" ++ bottom

expandAnnotated :: Monad m => ReferenceMap -> ReferenceID -> ReaderT Config m (Either TangleError T.Text)
expandAnnotated refs id = do
    cfg <- ask
    return $ expandGeneric (\id -> runReader (expandAnnotated refs id) cfg)
                           (\cb -> runReader (annotate id cb) cfg) refs id

tangleAnnotated :: Monad m => ReferenceMap -> ReaderT Config m FileMap
tangleAnnotated refs = do
    let fileRefs  = filter isFileReference (Map.keys refs)
        fileNames = map referenceName fileRefs
    sources <- mapM (expandAnnotated refs) fileRefs
    return $ Map.fromList $ zip fileNames sources

-- ========================================================================= --
-- Unit tests                                                                --
-- ========================================================================= --

tangleSpec :: Spec
tangleSpec = do
    describe "Tangle.codeLine" $
        it "just separates lines" $
            parse (many codeLine) "" "Hello\nWorld\n" `shouldBe`
                Right [SourceText "Hello", SourceText "World"]
    describe "Tangle.nowebReference" $ do
        it "picks out reference" $
            parse nowebReference "" "  <<test>>\n" `shouldBe`
                (Right $ NowebReference "test" "  ")
        it "skips anything that doesn't match" $
            parse nowebReference "" "meh <<test>>\n" `shouldSatisfy`
                isLeft    
