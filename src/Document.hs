-- ~\~ language=Haskell filename=src/Document.hs
-- ~\~ begin <<lit/02-document-model.md|src/Document.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Document
    ( module Document
    , module Errors ) where

import RIO
import RIO.List (sort)
import qualified RIO.Set as S
import qualified RIO.Map as M
import Errors

-- ~\~ begin <<lit/02-document-model.md|document-structure>>[0]
newtype ReferenceName = ReferenceName
    { unReferenceName :: Text
    } deriving (Show, Eq, Ord, Display)

data ReferenceId = ReferenceId
    { referenceFile       :: FilePath
    , referenceName       :: ReferenceName
    , referenceCount      :: Int
    } deriving (Show, Eq, Ord)

showNowebReference :: ReferenceName -> Text
showNowebReference (ReferenceName x) = "<<" <> x <> ">>"
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[1]
data Content
    = PlainText Text
    | Reference ReferenceId
    deriving (Show, Eq)

type ReferencePair = (ReferenceId, CodeBlock)
type ReferenceMap = Map ReferenceId CodeBlock
type FileMap = Map FilePath (ReferenceName, Text)

data Document = Document
    { references      :: ReferenceMap
    , documentContent :: [Content]
    , documentTargets :: FileMap
    } deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[2]
referenceNames :: ReferenceMap -> Set ReferenceName
referenceNames = S.fromList . map referenceName . M.keys

referencesByName :: ReferenceMap -> ReferenceName -> [ReferenceId]
referencesByName refs name
    = (sort . filter ((== name) . referenceName) . M.keys) refs

codeBlocksByName :: ReferenceMap -> ReferenceName -> [CodeBlock]
codeBlocksByName refs name = mapMaybe (refs M.!?) $ referencesByName refs name
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[3]
data CodeProperty
    = CodeId Text
    | CodeAttribute Text Text
    | CodeClass Text
    deriving (Eq, Show)

getAttribute :: [CodeProperty] -> Text -> Maybe Text
getAttribute [] _ = Nothing
getAttribute (CodeAttribute k v:ps) l
    | k == l    = Just v
    | otherwise = getAttribute ps l
getAttribute (_:ps) l = getAttribute ps l
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[4]
data CodeBlock = CodeBlock
    { codeLanguage   :: ProgrammingLanguage
    , codeProperties :: [CodeProperty]
    , codeSource     :: Text
    , codeLineNumber :: Maybe Int
    } deriving (Show, Eq)
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[5]
data ProgrammingLanguage
    = KnownLanguage Text
    | UnknownClass Text
    | NoLanguage
    deriving (Show, Eq)
-- ~\~ end
-- ~\~ end
