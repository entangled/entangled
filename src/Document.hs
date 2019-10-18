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
import Data.Maybe (mapMaybe)

-- ------ begin <<entangled-error>>[0]
data EntangledError
    = TangleError Text
    | UnknownError
    deriving (Show)
-- ------ end
-- ------ begin <<document-utils>>[0]
unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ------ end
-- ------ begin <<document-structure>>[0]
newtype ReferenceName = ReferenceName
    { unReferenceName :: Text
    } deriving (Show, Eq, Ord)

data ReferenceId = ReferenceId
    { referenceName :: ReferenceName
    , referenceCount :: Int
    } deriving (Show, Eq, Ord)
-- ------ end
-- ------ begin <<document-structure>>[1]
data Content
    = PlainText Text
    | Reference ReferenceId
    deriving (Show, Eq)

type ReferenceMap = Map ReferenceId CodeBlock
type FileMap = Map FilePath ReferenceName

data Document = Document
    { references      :: ReferenceMap
    , documentContent :: [Content]
    , files           :: FileMap
    } deriving (Show)
-- ------ end
-- ------ begin <<document-structure>>[2]

-- ------ end
-- ------ begin <<document-structure>>[3]
data CodeProperty
    = CodeId Text
    | CodeAttribute Text Text
    | CodeClass Text
    deriving (Eq, Show)
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
    = KnownLanguage Text
    | UnknownClass Text
    | NoLanguage
    deriving (Show, Eq)
-- ------ end
-- ------ end
