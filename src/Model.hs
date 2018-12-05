module Model
    ( TangleError(..)
    , toTangleError
    , ReferenceID(..)
    , ReferenceMap(..)
    , emptyReferenceMap
    , referenceName
    , findAllNamedReferences
    , countReferences
    , isFileReference
    , CodeBlock(..)
    , Content(..)
    , Document(..)
    , textToString
    , listFiles
    , stitchText
    ) where

import qualified Data.Map as Map
import Languages

newtype TangleError = TangleError String
        deriving (Eq, Show)

toTangleError :: Show e => Either e a -> Either TangleError a
toTangleError (Left err) = Left $ TangleError $ show err
toTangleError (Right x)  = Right x

{-|
  A code block may reference a filename or a noweb reference.
 -}
data ReferenceID  = FileReferenceID String
                  | NameReferenceID String Int
                  deriving (Show, Eq, Ord)

referenceName :: ReferenceID -> String
referenceName (FileReferenceID x) = x
referenceName (NameReferenceID x _) = x

data CodeBlock = CodeBlock
    { codeLanguage  :: String
    , codeSource    :: String
    } deriving (Show, Eq)

{-|
  Each 'ReferenceID' connects to a 'CodeBlock'
 -}
type ReferenceMap = Map.Map ReferenceID CodeBlock

emptyReferenceMap :: ReferenceMap
emptyReferenceMap = Map.fromList []

findAllNamedReferences :: String -> ReferenceMap -> [ReferenceID]
findAllNamedReferences name = filter ((== name) . referenceName) . Map.keys

countReferences :: String -> ReferenceMap -> Int
countReferences name refs = length $ findAllNamedReferences name refs

{-|
  Any piece of 'Content' is a 'RawText' or 'Reference'.
 -}
data Content = RawText String
             | Reference ReferenceID
             deriving (Show, Eq)

{-|
  A document is a list of 'Text' and a 'ReferenceMap'.
 -}
data Document = Document
    { references      :: ReferenceMap
    , documentContent :: [Content]
    } deriving (Show)

textToString :: ReferenceMap -> Content -> String
textToString ref (RawText x) = x
textToString ref (Reference r) = code
    where CodeBlock lang code = ref Map.! r

stitchText :: Document -> String
stitchText (Document ref txt) =
    concatMap ((++ "\n") . textToString ref) txt

isFileReference :: ReferenceID -> Bool
isFileReference (FileReferenceID _) = True
isFileReference _ = False

listFiles :: Document -> [ReferenceID]
listFiles (Document refs _) = filter isFileReference $ Map.keys refs
