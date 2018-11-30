module Model
    ( ReferenceID(..)
    , ReferenceMap(..)
    , emptyReferenceMap
    , referenceName
    , isFileReference
    , Language(..)
    , CodeBlock(..)
    , Text(..)
    , Document(..)
    , textToString
    , stitchText
    ) where

import qualified Data.Map as Map

{-|
  A code block may reference a filename or a noweb reference.
 -}
data ReferenceID  = FileReferenceID String
                  | NameReferenceID String
                  deriving (Show, Eq, Ord)

referenceName (FileReferenceID x) = x
referenceName (NameReferenceID x) = x

newtype Language = Language String
    deriving (Show, Eq)

data CodeBlock = CodeBlock Language String
    deriving (Show, Eq)

{-|
  Each 'ReferenceID' connects to a 'CodeBlock'
 -}
type ReferenceMap = Map.Map ReferenceID CodeBlock

emptyReferenceMap :: ReferenceMap
emptyReferenceMap = Map.fromList []

{-|
  Any piece of 'Text' is a 'RawText' or 'Reference'.
 -}
data Text = RawText String
          | Reference ReferenceID
          deriving (Show, Eq)

{-|
  A document is a list of 'Text' and a 'ReferenceMap'.
 -}
data Document = Document
    { references :: ReferenceMap
    , text       :: [Text]
    }
    deriving (Show)

textToString :: ReferenceMap -> Text -> String
textToString ref (RawText x) = x
textToString ref (Reference r) = code
    where CodeBlock lang code = ref Map.! r

stitchText :: Document -> String
stitchText (Document ref txt) =
    concatMap ((++ "\n") . textToString ref) txt

isFileReference :: ReferenceID -> Bool
isFileReference (FileReferenceID _) = True
isFileReference _ = False
