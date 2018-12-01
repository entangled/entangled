module Tangle
    ( Source(..)
    , nowebReference
    , codeLine
    , tangleNaked
    , tangleAnnotated
    ) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader

import Text.Parsec

import Model
import Config
import Languages

newtype TangleError = TangleError String
        deriving (Show)

type FileMap = Map.Map String (Either TangleError String)

data Source = SourceText String 
            | NowebReference String String
            deriving (Eq, Show)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorval Nothing = Left errorval
maybeToEither _ (Just normalval) = Right normalval

nowebReference :: Parsec String b Source
nowebReference = do
    indent <- many (oneOf " \t")
    string "<<"
    id <- many1 $ noneOf " \t<>"
    string ">>"
    many (oneOf " \t")
    endOfLine
    return $ NowebReference id indent

skip :: Parsec String b c -> Parsec String b ()
skip a = do
    x <- a
    return ()

line :: Parsec String b String
line = manyTill anyChar (try endOfLine)

codeLine :: Parsec String b Source
codeLine = SourceText <$> line

nowebCode :: Parsec String b [Source]
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
lookupLanguage :: String -> [Language] -> Either TangleError Language
lookupLanguage x langs = maybeToEither
    (TangleError $ "Unknown language marker: " ++ x)
    $ find (elem x . languageAbbreviations) langs

topAnnotation :: ReferenceID -> String -> String -> String
topAnnotation (NameReferenceID n i) comment lang =
    comment ++ " begin <<" ++ n ++ ">>[" ++ show i ++ "]"
topAnnotation (FileReferenceID n) comment lang =
    comment ++ " language=\"" ++ lang ++ "\" file=\"" ++ n ++ "\""

annotate :: [Language] -> ReferenceID -> CodeBlock -> Either TangleError String
annotate languages id (CodeBlock (LanguageId lang) text) = do
    (Language langName _ comment) <- lookupLanguage lang languages
    let top      = topAnnotation id comment langName
        bottom   = comment ++ " end"
    return $ top ++ "\n" ++ text ++ "\n" ++ bottom

expandAnnotated :: [Language] -> ReferenceMap -> ReferenceID -> Either TangleError String
expandAnnotated langs refs id = expandGeneric (expandAnnotated langs refs) (annotate langs id) refs id

tangleAnnotated :: Document -> Reader Config FileMap
tangleAnnotated (Document refs _) = do
    langs <- asks configLanguages
    let fileRefs  = filter isFileReference (Map.keys refs)
        fileNames = map referenceName fileRefs
        sources = map (expandAnnotated langs refs) fileRefs
    return $ Map.fromList $ zip fileNames sources
