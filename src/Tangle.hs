module Tangle
    ( tangle
    ) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Extra

import Text.Parsec

import Model

newtype TangleError = TangleError String
        deriving (Show)

type FileMap = Map.Map String (Either TangleError String)

data Source = SourceText String 
            | NowebReference String String

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither errorval Nothing = Left errorval
maybeToEither _ (Just normalval) = Right normalval

nowebReference :: Parsec String b Source
nowebReference = do
    indent <- many space
    string "<<"
    id <- many1 $ noneOf " <>"
    string ">>"
    spaces
    endOfLine
    return $ NowebReference id indent

skip :: Parsec String b c -> Parsec String b ()
skip a = do
    x <- a
    return ()

line :: Parsec String b String
line = manyTill anyChar (try $ skip endOfLine <|> eof)

codeLine :: Parsec String b Source
codeLine = SourceText <$> line

nowebCode :: Parsec String b [Source]
nowebCode = many (nowebReference <|> codeLine)

parseCode :: ReferenceID -> String -> Either TangleError [Source]
parseCode id x = case parse nowebCode (referenceName id) x of
    Left  err -> Left $ TangleError $ show err
    Right src -> Right src

sourceToString :: ReferenceMap -> Source -> Either TangleError String
sourceToString _    (SourceText x) = Right x
sourceToString refs (NowebReference r _) = 
    expandReference refs $ NameReferenceID r

codeToString :: ReferenceMap -> [Source] -> Either TangleError String
codeToString refs = concatMapM (fmap (++ "\n") . sourceToString refs)

expandReference :: ReferenceMap -> ReferenceID -> Either TangleError String
expandReference refs id = do
    CodeBlock _ text <- maybeToEither 
                (TangleError $ "Reference " ++ show id ++ " not found.")
                (refs Map.!? id)
    code <- parseCode id text
    codeToString refs code

tangle :: Document -> FileMap
tangle (Document refs _) = Map.fromList $ zip fileNames sources
    where fileRefs  = filter isFileReference (Map.keys refs)
          sources   = map (expandReference refs) fileRefs
          fileNames = map referenceName fileRefs
