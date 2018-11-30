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

codeLineToString :: Expander -> Source -> Either TangleError String
codeLineToString _ (SourceText x) = Right x
codeLineToString e (NowebReference r i) = 
    addIndent i <$> e (NameReferenceID r)

codeListToString :: Expander -> [Source] -> Either TangleError String
codeListToString e = concatMapM (fmap (++ "\n") . codeLineToString e)

expandNaked :: ReferenceMap -> ReferenceID -> Either TangleError String
expandNaked refs id = do
    CodeBlock lang text <- maybeToEither 
                (TangleError $ "Reference " ++ show id ++ " not found.")
                (refs Map.!? id)
    code <- parseCode id $ text ++ "\n"
    codeListToString (expandNaked refs) code

tangleNaked :: Document -> FileMap
tangleNaked (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandNaked refs) fileRefs
          fileNames = map referenceName fileRefs

annotate :: CodeBlock -> Reader Config String
annotate (CodeBlock lang text) = do
    languages <- asks configLanguages
    
tangleAnnotated :: Document -> Reader Config FileMap
tangleAnnotated (Document refs _) = do
