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
import Data.List (sort)

import Config (Language)
import TextUtil (tshow)

-- ------ begin <<entangled-error>>[0]
data EntangledError
    = TangleError Text
    | StitchError Text
    | CyclicReference Text
    | UnknownLanguageClass Text
    | MissingLanguageClass
    | UnknownError
    deriving (Show, Ord, Eq)

toEntangledError :: (Show e)
                 => (Text -> EntangledError) -> Either e a
                 -> Either EntangledError a
toEntangledError _ (Right x) = Right x
toEntangledError f (Left x) = Left $ f $ tshow x
-- ------ end

-- ------ begin <<document-structure>>[0]
newtype ReferenceName = ReferenceName
    { unReferenceName :: Text
    } deriving (Show, Eq, Ord)

data ReferenceId = ReferenceId
    { referenceName :: ReferenceName
    , referenceCount :: Int
    } deriving (Show, Eq, Ord)

nowebReference :: ReferenceName -> Text
nowebReference (ReferenceName x) = "<<" <> x <> ">>"
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
    , files           :: FileMap
    } deriving (Show)
-- ------ end
-- ------ begin <<document-structure>>[2]
referenceNames :: Document -> Set ReferenceName
referenceNames = S.fromList . map referenceName . M.keys . references

referencesByName :: Document -> ReferenceName -> [ReferenceId]
referencesByName doc name
    = (sort . filter ((== name) . referenceName) . M.keys . references) doc

codeBlocksByName :: Document -> ReferenceName -> [CodeBlock]
codeBlocksByName doc name = map (references doc M.!) $ referencesByName doc name
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
