-- ~\~ language=Haskell filename=src/Document.hs
-- ~\~ begin <<lit/02-document-model.md|src/Document.hs>>[0]
module Document
    ( module Document
    , module Errors ) where

-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|import-map>>[0]
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|import-set>>[0]
import qualified Data.Set as S
import Data.Set (Set)
-- ~\~ end
import Data.Typeable (Typeable)
import Data.List (sort)

import Control.Monad.Catch

import TextUtil (tshow)
import Errors

-- ~\~ begin <<lit/02-document-model.md|document-structure>>[0]
newtype ReferenceName = ReferenceName
    { unReferenceName :: Text
    } deriving (Show, Eq, Ord)

data ReferenceId = ReferenceId
    { referenceFile :: FilePath
    , referenceName :: ReferenceName
    , referenceCount :: Int
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
type FileMap = Map FilePath (ReferenceName, Text)  -- map to ref and language

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
codeBlocksByName refs name = map (refs M.!) $ referencesByName refs name
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
    } deriving (Show, Eq)
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[5]
data ProgrammingLanguage
    = KnownLanguage Text
    | UnknownClass Text
    | NoLanguage
    deriving (Show, Eq)
-- ~\~ end
-- ~\~ begin <<lit/02-document-model.md|document-structure>>[6]
getCodeClasses :: CodeBlock -> [Text]
getCodeClasses CodeBlock{..} = classes codeProperties
    where classes [] = []
          classes (CodeClass c : cs) = c : classes cs
          classes (_ : cs) = classes cs

getCodeAttributes :: CodeBlock -> [(Text, Text)]
getCodeAttributes CodeBlock{..} = attrs codeProperties
    where attrs [] = []
          attrs (CodeAttribute k v : cs) = (k, v) : attrs cs
          attrs (_ : cs) = attrs cs
-- ~\~ end
-- ~\~ end
