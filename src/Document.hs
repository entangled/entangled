-- ------ language="Haskell" file="src/Document.hs"
module Document where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-map>>[0]
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end
-- ------ begin <<import-set>>[0]
import qualified Data.Set as S
import Data.Set (Set)
-- ------ end
import Data.Typeable (Typeable)
import Data.List (sort)

import Control.Monad.Catch

import Config (Language)
import TextUtil (tshow)

-- ------ begin <<entangled-error>>[0]
data EntangledError
    = TangleError Text
    | StitchError Text
    | CyclicReference Text
    | UnknownLanguageClass Text
    | DatabaseError Text
    | MissingLanguageClass
    | UnknownError
    deriving (Show, Ord, Eq, Typeable)

toEntangledError :: (Show e)
                 => (Text -> EntangledError) -> Either e a
                 -> Either EntangledError a
toEntangledError _ (Right x) = Right x
toEntangledError f (Left x) = Left $ f $ tshow x

instance Exception EntangledError
-- ------ end

-- ------ begin <<document-structure>>[0]
newtype ReferenceName = ReferenceName
    { unReferenceName :: Text
    } deriving (Show, Eq, Ord)

data ReferenceId = ReferenceId
    { referenceName :: ReferenceName
    , referenceCount :: Int
    } deriving (Show, Eq, Ord)

showNowebReference :: ReferenceName -> Text
showNowebReference (ReferenceName x) = "<<" <> x <> ">>"
-- ------ end
-- ------ begin <<document-structure>>[1]
data Content
    = PlainText Text
    | Reference ReferenceId
    deriving (Show, Eq)

type ReferencePair = (ReferenceId, CodeBlock)
type ReferenceMap = Map ReferenceId CodeBlock
type FileMap = Map FilePath ReferenceName

data Document = Document
    { references      :: ReferenceMap
    , documentContent :: [Content]
    , documentTargets :: FileMap
    } deriving (Show)
-- ------ end
-- ------ begin <<document-structure>>[2]
referenceNames :: ReferenceMap -> Set ReferenceName
referenceNames = S.fromList . map referenceName . M.keys

referencesByName :: ReferenceMap -> ReferenceName -> [ReferenceId]
referencesByName refs name
    = (sort . filter ((== name) . referenceName) . M.keys) refs

codeBlocksByName :: ReferenceMap -> ReferenceName -> [CodeBlock]
codeBlocksByName refs name = map (refs M.!) $ referencesByName refs name
-- ------ end
-- ------ begin <<document-structure>>[3]
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
-- ------ begin <<document-structure>>[4]
data CodeBlock = CodeBlock
    { codeLanguage   :: ProgrammingLanguage
    , codeProperties :: [CodeProperty]
    , codeSource     :: Text
    } deriving (Show, Eq)
-- ------ end
-- ------ begin <<document-structure>>[5]
data ProgrammingLanguage
    = KnownLanguage Language
    | UnknownClass Text
    | NoLanguage
    deriving (Show, Eq)
-- ------ end
-- ------ end
