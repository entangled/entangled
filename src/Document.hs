-- ------ language="Haskell" file="src/Document.hs" project://lit/02-document-model.md#4
module Document
    ( module Document
    , module Errors ) where

-- ------ begin <<import-text>>[0] project://lit/01-entangled.md#44
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-map>>[0] project://lit/01-entangled.md#24
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
-- ------ begin <<import-set>>[0] project://lit/01-entangled.md#35
import qualified Data.Set as S
import Data.Set (Set)
-- ------ end
import Data.Typeable (Typeable)
import Data.List (sort)

import Control.Monad.Catch

import TextUtil (tshow)
import Errors

-- ------ begin <<document-structure>>[0] project://lit/02-document-model.md#54
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
-- ------ end
-- ------ begin <<document-structure>>[1] project://lit/02-document-model.md#73
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
-- ------ end
-- ------ begin <<document-structure>>[2] project://lit/02-document-model.md#92
referenceNames :: ReferenceMap -> Set ReferenceName
referenceNames = S.fromList . map referenceName . M.keys

referencesByName :: ReferenceMap -> ReferenceName -> [ReferenceId]
referencesByName refs name
    = (sort . filter ((== name) . referenceName) . M.keys) refs

codeBlocksByName :: ReferenceMap -> ReferenceName -> [CodeBlock]
codeBlocksByName refs name = map (refs M.!) $ referencesByName refs name
-- ------ end
-- ------ begin <<document-structure>>[3] project://lit/02-document-model.md#116
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
-- ------ end
-- ------ begin <<document-structure>>[4] project://lit/02-document-model.md#133
data CodeBlock = CodeBlock
    { codeLanguage   :: ProgrammingLanguage
    , codeProperties :: [CodeProperty]
    , codeSource     :: Text
    } deriving (Show, Eq)
-- ------ end
-- ------ begin <<document-structure>>[5] project://lit/02-document-model.md#143
data ProgrammingLanguage
    = KnownLanguage Text
    | UnknownClass Text
    | NoLanguage
    deriving (Show, Eq)
-- ------ end
-- ------ begin <<document-structure>>[6] project://lit/02-document-model.md#153
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
-- ------ end
-- ------ end
