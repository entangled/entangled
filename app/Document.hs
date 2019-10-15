-- ------ language="Haskell" file="app/Document.hs"
module Document
    () where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
-- ------ begin <<import-map>>[0]
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
-- ------ end

-- ------ begin <<document-utils>>[0]
unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ------ end
-- ------ begin <<document-structure>>[0]
data ReferenceId = FileReferenceId Text
                 | NameReferenceId Text Int
                 deriving (Show, Eq, Ord)
-- ------ end
-- ------ begin <<document-structure>>[1]
data Content
    = PlainText Text
    | Reference ReferenceId
    deriving (Show, Eq)

type ReferencePair = (ReferenceId, CodeBlock)
type ReferenceMap = Map ReferenceId CodeBlock

data Document = Document
    { references      :: ReferenceMap
    , documentContent :: [Content]
    } deriving (Show)
-- ------ end
-- ------ begin <<document-structure>>[2]
referenceName :: ReferenceId -> Maybe Text
referenceName (FileReferenceId _) = Nothing
referenceName (NameReferenceId n _) = n

allNameReferences :: Text -> ReferenceMap -> [ReferenceId]
allNameReferences name = filter ((== Just name) . referenceName) . M.keys

countReferences :: Text -> ReferenceMap -> Int
countReferences name refs = length $ allNameReferences name refs

isFileReference :: ReferenceId -> Bool
isFileReference (FileReferenceId _) = True
isFileReference _ = False

listFiles :: Document -> [ReferenceId]
listFiles (Document refs _) = filter isFileReference $ M.keys refs

listActiveReferences :: Document -> [ReferenceId]
listActiveReferences doc = mapMaybe getReference (documentContent doc)
    where getReference (PlainText _) = Nothing
          getReference (Reference r) = Just r
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
-- ------ end
-- ------ end
