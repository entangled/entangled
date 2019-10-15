# Document structure

``` {.haskell file=app/Document.hs}
module Document
    () where

<<import-text>>
<<import-map>>

<<document-utils>>
<<document-structure>>
```

## Utils

We need a version of `unlines` that doesn't append a final newline.

``` {.haskell #document-utils}
unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
```

This way, `unlines'` has nicer properties. No single `Text` in Entangled ends in a newline, meaning

``` {.haskell}
unlines' [unlines' a, unlines' b] = unlines' (a <> b)
```

## Structure

A document is modelled as a series of text and code blocks. A code block is delimited by two lines starting with three backtics:

~~~
 Normal text.

 ```
 print("this is a code block")
 ```
~~~

Each code block that is of relevance to Entangled has a reference attached. This is either the `file` attribute,

~~~
 ``` {.language file=filename}
 ```
~~~

or an identifier.

~~~
 ``` {.language #identifier}
 ```
~~~

An identifier may be repeated, in which case the code block is concatenated to the previous instances. Each instance will have an associated integer marking the place in the sequence. 

### Reference Id
We define the type `ReferenceId`.

``` {.haskell #document-structure}
data ReferenceId = FileReferenceId Text
                 | NameReferenceId Text Int
                 deriving (Show, Eq, Ord)
```

The type is deriving `Ord`, so that we can use it as an index for a `Map`.


### Document
Using the `ReferenceId` type we can represent a text (or `Document`) as a sequence of plain `Text` or `ReferenceId`, paired up with a map linking each `ReferenceId` with a code block.

``` {.haskell #document-structure}
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
```

### Accessors

``` {.haskell #document-structure}
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
```

### Code blocks

A code block can have all attributes that are normally associated in the context of CSS. The following markdown,

~~~
 ``` {.class #id attribute=value}
 source
 ```
~~~

can be mapped to a list of `CodeProperty`.

``` {.haskell #document-structure}
data CodeProperty
    = CodeId Text
    | CodeAttribute Text Text
    | CodeClass Text
    deriving (Eq, Show)
```

In our case the *class* represents the code language or an abbreviation thereof.

``` {.haskell #document-structure}
data CodeBlock = CodeBlock
    { codeLanguage   :: ProgrammingLanguage
    , codeProperties :: [CodeProperty]
    , codeSource     :: Text
    } deriving (Show, Eq)
```

A `ProgrammingLanguage` is either a known language (for which we know how to generate comments) an unknown (or misspelled) class identifier, or empty.

``` {.haskell #document-structure}
data ProgrammingLanguage
    = KnownLanguage Text
    | UnknownClass Text
    | NoLanguage
```

## Conversion

We can convert a reference back to text by looking up the reference. If the code block is emtpy this returns `Nothing`. This is due to the choice not to include the final newline in the code block.

``` {.haskell #document-conversion}
contentToText :: ReferenceMap -> Content -> Maybe Text
contentToText ref (PlainText x) = Just x
contentToText ref (Reference r) 
    | code == "" = Nothing
    | otherwise  = Just code
    where CodeBlock _ _ code = ref M.! r
```

Stitching text back together:

``` {.haskell #document-conversion}
stitchText :: Document -> Text
stitchText (Document refs c) = T.unlines $ mapMaybe (contentToText refs) c
```

