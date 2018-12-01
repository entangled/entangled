module Tangle
    ( Source(..)
    , nowebReference
    , codeLine
    , tangleNaked
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

expandNaked :: ReferenceMap -> ReferenceID -> Either TangleError String
expandNaked refs id = do
    CodeBlock lang text <- maybeToEither 
                (TangleError $ "Reference " ++ show id ++ " not found.")
                (refs Map.!? id)
    code <- parseCode id $ text ++ "\n"
    codeListToString refs (expandNaked refs) code

tangleNaked :: Document -> FileMap
tangleNaked (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandNaked refs) fileRefs
          fileNames = map referenceName fileRefs

{- Annotated tangle -}
lookupComment :: String -> [Language] -> Either TangleError String
lookupComment x = maybeToEither
    (TangleError $ "Unknown language marker: " ++ x)
    languageLineComment <$> find 
        (\l -> x `member` languageAbbreviations l)

topAnnotation :: ReferenceID -> String -> String
topAnnotation (NameReferenceID n i) comment =
    comment ++ " " ++ "<<" ++ n ++ ">>" ++ if i == 0 then "=" else "+="
topAnnotation (FileReferenceID n) comment =
    comment ++ " " ++ "file=" ++ n

annotate :: ReferenceID -> CodeBlock -> Reader Config (Either TangleError String)
annotate id (CodeBlock lang text) = do
    languages <- asks configLanguages
    return $ do
        comment <- lookupComment lang languages
        return $ topAnnotation id comment ++ "\n" ++ text

expandAnnotated :: ReferenceMap -> ReferenceID
tangleAnnotated :: Document -> Reader Config FileMap
tangleAnnotated (Document refs _) = do
