{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Model (TangleError(..), toTangleError)
import Document
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

parseCode :: ReferenceId -> String -> Either TangleError [Source]
parseCode id x = case parse nowebCode (referenceName id) x of
    Left  err -> Left $ TangleError $ show err
    Right src -> Right src

addIndent :: String -> T.Text -> T.Text
addIndent indent text 
    = T.intercalate (T.pack "\n") 
    $ map (T.append (T.pack indent)) $ T.lines text

type History = [ReferenceId]
type Expander = History -> ReferenceId -> Either TangleError T.Text

codeLineToString :: ReferenceMap -> Expander -> History -> Source -> Either TangleError T.Text
codeLineToString _ _ _ (SourceText x) = Right $ T.pack x
codeLineToString refs e h (NowebReference r i) = 
    addIndent i . T.concat <$> mapM (e h) (allNameReferences r refs)

codeListToString :: ReferenceMap -> Expander -> History -> [Source] -> Either TangleError T.Text
codeListToString refs e h srcs = T.unlines <$> mapM (codeLineToString refs e h) srcs

expandGeneric :: Expander -> (CodeBlock -> Either TangleError T.Text) -> ReferenceMap -> History -> ReferenceId -> Either TangleError T.Text
expandGeneric e codeToText refs h id = do
    when (id `elem` h) $ Left $ CyclicReference $ show id ++ " in " ++ show h
    codeBlock <- maybeToEither 
        (TangleError $ "Reference " ++ show id ++ " not found.")
        (refs Map.!? id)
    text <- codeToText codeBlock
    parsedCode <- parseCode id $ T.unpack text ++ "\n"
    codeListToString refs e (id : h) parsedCode

expandNaked :: ReferenceMap -> History -> ReferenceId -> Either TangleError T.Text
expandNaked refs = expandGeneric (expandNaked refs) (Right . codeSource) refs

tangleNaked :: Document -> FileMap
tangleNaked (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandNaked refs []) fileRefs
          fileNames = map referenceName fileRefs

{- Annotated tangle -}
topAnnotation :: ReferenceId -> String -> String -> String
topAnnotation (NameReferenceId n i) comment lang =
    comment ++ " begin <<" ++ n ++ ">>[" ++ show i ++ "]"
topAnnotation (FileReferenceId n) comment lang =
    comment ++ " language=\"" ++ lang ++ "\" file=\"" ++ n ++ "\""

lookupLanguage' :: (MonadReader Config m)
        => String -> m (Either TangleError Language)
lookupLanguage' name = do
    lang <- reader $ languageFromName name
    case lang of
        Nothing -> return $ Left $ TangleError $ "unknown language: " ++ name
        Just l  -> return $ Right l

annotate :: (MonadReader Config m)
        => ReferenceId -> CodeBlock -> m (Either TangleError T.Text)
annotate id (CodeBlock lang _ text) = do
    l <- lookupLanguage' lang
    return $ do 
        (Language _ _ comment) <- l
        let top      = T.pack $ topAnnotation id comment lang
            bottom   = T.pack $ comment ++ " end"
        return $ top <> T.pack "\n" <> text <> T.pack "\n" <> bottom

expandAnnotated :: (MonadReader Config m)
        => ReferenceMap -> History -> ReferenceId -> m (Either TangleError T.Text)
expandAnnotated refs h id = do
    cfg <- ask
    return $ expandGeneric (\h' id' -> runReader (expandAnnotated refs h' id') cfg)
                           (\cb -> runReader (annotate id cb) cfg) refs h id

tangleAnnotated :: (MonadReader Config m) => ReferenceMap -> m FileMap
tangleAnnotated refs = do
    let fileRefs  = filter isFileReference (Map.keys refs)
        fileNames = map referenceName fileRefs
    sources <- mapM (expandAnnotated refs []) fileRefs
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
