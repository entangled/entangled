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

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), manyTill, anyChar, try, endOfLine, many, oneOf, noneOf, string, many1)

import Model
import Config
import Languages

import Test.Hspec
import Data.Either.Combinators (fromRight')
import Data.Either

type FileMap = Map.Map String (Either TangleError String)

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

addIndent :: String -> String -> String
addIndent indent text = intercalate "\n" $ map (indent ++) $ lines text

type Expander = ReferenceID -> Either TangleError String

codeLineToString :: ReferenceMap -> Expander -> Source -> Either TangleError String
codeLineToString _ _ (SourceText x) = Right x
codeLineToString refs e (NowebReference r i) = 
    addIndent i . concat <$> mapM e (findAllNamedReferences r refs)

codeListToString :: ReferenceMap -> Expander -> [Source] -> Either TangleError String
codeListToString refs e = concatMapM (fmap (++ "\n") . codeLineToString refs e)

expandGeneric :: Expander -> (CodeBlock -> Either TangleError String) -> ReferenceMap
              -> ReferenceID -> Either TangleError String
expandGeneric e codeToString refs id = do
    codeBlock <- maybeToEither 
        (TangleError $ "Reference " ++ show id ++ " not found.")
        (refs Map.!? id)
    text <- codeToString codeBlock
    parsedCode <- parseCode id $ text ++ "\n"
    codeListToString refs e parsedCode

expandNaked :: ReferenceMap -> ReferenceID -> Either TangleError String
expandNaked refs = expandGeneric (expandNaked refs) (Right . codeSource) refs

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

annotate :: Monad m => ReferenceID -> CodeBlock -> ReaderT Config m (Either TangleError String)
annotate id (CodeBlock lang text) = do
    l <- lookupLanguage' lang
    return $ do 
        (Language _ _ comment) <- l
        let top      = topAnnotation id comment lang
            bottom   = comment ++ " end"
        return $ top ++ "\n" ++ text ++ "\n" ++ bottom

expandAnnotated :: Monad m => ReferenceMap -> ReferenceID -> ReaderT Config m (Either TangleError String)
expandAnnotated refs id = do
    cfg <- ask
    return $ expandGeneric (\id -> runReader (expandAnnotated refs id) cfg)
                           (\cb -> runReader (annotate id cb) cfg) refs id

tangleAnnotated :: Monad m => Document -> ReaderT Config m FileMap
tangleAnnotated (Document refs _) = do
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
