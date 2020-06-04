# Configuration

The configuration is written in Dhall and has the following schema:

``` {.dhall file=data/config-schema.dhall}
let Comment : Type = < Line : Text | Block : { start : Text, end : Text } >
let Language : Type = { name : Text, identifiers : List Text, comment : Comment }
let LineDirective : Type = { name : Text, format: Text }

<<config-comment-styles>>
<<config-languages>>

let Annotate = < Naked | Standard | Project >

let Config =
    { Type =
        { version   : Text
        , languages : List Language
        , watchList : List Text
        , database  : Optional Text
        , annotate  : Annotate
        , lineDirectives : List LineDirective
        , useLineDirectives : Bool }
    , default =
        { version   = "1.0.0"
        , languages = languages
        , watchList = [] : List Text
        , database  = None Text
        , annotate  = Annotate.Standard
        , lineDirectives = lineDirectives
        , useLineDirectives = False }
    }

in { Comment   = Comment
   , Language  = Language
   , LineDirective = LineDirective
   , Config    = Config
   , Annotate  = Annotate
   , comments  = comments
   , languages = languages
   , lineDirectives = lineDirectives
   }
```

## Languages

``` {.dhall #config-comment-styles}
let comments =
    { hash         = Comment.Line "#"
    , lispStyle    = Comment.Line ";"
    , cStyle       = Comment.Block { start = "/*", end = "*/" }
    , cppStyle     = Comment.Line "//"
    , haskellStyle = Comment.Line "--"
    , mlStyle      = Comment.Block { start = "(*", end = "*)" }
    , xmlStyle     = Comment.Block { start = "<!--", end = "-->" }
    , texStyle     = Comment.Line "%"
    }
```

``` {.dhall #config-languages}
let languages =
    [ { name = "Awk",        identifiers = ["awk"],           comment = comments.hash }
    , { name = "C",          identifiers = ["c"],             comment = comments.cStyle }
    , { name = "C++",        identifiers = ["cpp", "c++"],    comment = comments.cppStyle }
    , { name = "Clojure",    identifiers = ["clojure"],       comment = comments.lispStyle }
    , { name = "CSS",        identifiers = ["css"],           comment = comments.cStyle }
    , { name = "D",          identifiers = ["d"],             comment = comments.cppStyle }
    , { name = "Dhall",      identifiers = ["dhall"],         comment = comments.haskellStyle }
    , { name = "Elm",        identifiers = ["elm"],           comment = comments.haskellStyle }
    , { name = "Gnuplot",    identifiers = ["gnuplot"],       comment = comments.hash }
    , { name = "Haskell",    identifiers = ["haskell"],       comment = comments.haskellStyle }
    , { name = "HTML",       identifiers = ["html"],          comment = comments.xmlStyle }
    , { name = "Idris",      identifiers = ["idris"],         comment = comments.haskellStyle }
    , { name = "Julia",      identifiers = ["julia"],         comment = comments.hash }
    , { name = "JavaScript", identifiers = ["js", "javascript", "ecma"],
                                                              comment = comments.cStyle }
    , { name = "LaTeX",      identifiers = ["latex"],         comment = comments.texStyle }
    , { name = "Lua",        identifiers = ["lua"],           comment = comments.haskellStyle }
    , { name = "Make",       identifiers = ["make", "makefile"],
                                                              comment = comments.hash }
    , { name = "OCaml",      identifiers = ["ocaml"],         comment = comments.mlStyle }
    , { name = "OpenCL",     identifiers = ["opencl"],        comment = comments.cStyle }
    , { name = "PureScript", identifiers = ["purs", "purescript"],
                                                              comment = comments.haskellStyle }
    , { name = "Python",     identifiers = ["py", "python"],  comment = comments.hash }
    , { name = "R",          identifiers = ["r"],             comment = comments.hash }
    , { name = "Rust",       identifiers = ["rust"],          comment = comments.cppStyle }
    , { name = "Scheme",     identifiers = ["scheme", "r6rs" ,"racket", "r7rs"],
                                                              comment = comments.lispStyle }
    , { name = "SQLite",     identifiers = ["sqlite"],        comment = comments.haskellStyle }
    , { name = "TOML",       identifiers = ["toml"],          comment = comments.hash }
    , { name = "TypeScript", identifiers = ["ts", "typescript"],
                                                              comment = comments.cppStyle }
    , { name = "YAML",       identifiers = ["yaml"],          comment = comments.hash }
    ]

let lineDirectives =
    [ { name = "C",          format = "#line {linenumber} \"{filename}\"" }
    , { name = "C++",        format = "#line {linenumber} \"{filename}\"" }
    , { name = "Haskell",    format = "{{-# LINE {linenumber} \"{filename}\" #-}}" }
    ]
```

## Reading config

``` {.haskell #config-import}
import Dhall (FromDhall, ToDhall, input, auto, Decoder, record, list
             , field, setFromDistinctList, constructor, unit, union)
import qualified Data.Text as T
```

We need to match the Dhall schema with types in Haskell

``` {.haskell #config-dhall-schema}
data ConfigComment
    = Line  Text
    | Block { start :: Text, end :: Text }
    deriving (Generic, Show)

instance FromDhall ConfigComment
instance ToDhall ConfigComment

data ConfigLanguage = ConfigLanguage
    { languageName :: Text
    , languageIdentifiers :: [Text]
    , languageComment :: ConfigComment
    } deriving (Show)

configLanguage :: Decoder ConfigLanguage
configLanguage = record
    ( ConfigLanguage <$> field "name"        auto
                     <*> field "identifiers" auto
                     <*> field "comment"     auto
    )

instance Eq ConfigLanguage where
    a == b = languageName a == languageName b

instance Ord ConfigLanguage where
    compare a b = compare (languageName a) (languageName b)

decodeFormatSpec :: Decoder Format.Spec
decodeFormatSpec = fromMaybe [Format.Plain "illegal format spec"] . Format.spec <$> auto

lineDirectivesDecoder :: Decoder (Map Text Format.Spec)
lineDirectivesDecoder = M.fromList <$> list entry
    where entry = record ( pair <$> field "name" auto
                                <*> field "format" decodeFormatSpec )
          pair a b = (a, b)

data AnnotateMethod = AnnotateNaked
                    | AnnotateStandard
                    | AnnotateProject
                    deriving (Show, Eq)

annotateDecoder :: Decoder AnnotateMethod
annotateDecoder = union
        (  ( AnnotateNaked          <$ constructor "Naked" unit )
        <> ( AnnotateStandard       <$ constructor "Standard" unit )
        <> ( AnnotateProject        <$ constructor "Project" unit ) )

data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    } deriving (Show)

configDecoder :: Decoder Config
configDecoder = record
    ( Config <$> field "version" auto
             <*> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
             <*> field "annotate" annotateDecoder
             <*> field "lineDirectives" lineDirectivesDecoder
             <*> field "useLineDirectives" auto
    )

class HasConfig env where
    config :: Lens' env Config
```

``` {.haskell file=src/Config.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import RIO hiding (void)
import qualified RIO.Map as M
<<config-import>>

import Errors
import qualified Format
import Data.List (find, scanl1)
import Control.Monad.Extra (concatMapM)
import System.FilePath.Glob (glob)
import System.Directory
import System.FilePath

<<config-dhall-schema>>
<<config-input>>
<<config-reader>>

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> throwM $ SystemError "database not configured"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ concatMapM (glob . T.unpack) (configWatchList cfg)
```

> ~~Configuration can be stored in `${XDG_CONFIG_HOME}/entangled/config.dhall`. Also the local directory or its parents may contain a `.entangled.dhall` file. These override settings in the global configuration.~~
> This is a bad idea. Configurations should work the same for people cloning a repository. Without this idea, also stacking of configurations is not needed. Dhall should handle all that.

There are currently no customisation options for entangled, but I keep my options open: namespaces for references, enabling future features like git support, you name it.

### Reading config files

``` {.haskell #config-input}
findFileAscending :: String -> IO (Maybe FilePath)
findFileAscending filename = do
    path <- dropTrailingPathSeparator <$> getCurrentDirectory
    let parents = reverse $ scanl1 (</>) $ splitDirectories path
    findFile parents filename

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- findFileAscending "entangled.dhall"
            >>= maybe (throwM $ SystemError "no config found") return
    input configDecoder $ "(" <> T.pack cfg_path <> ").entangled"
```

## Processing

``` {.haskell #config-reader}
lookupLanguage :: Config -> Text -> Maybe ConfigLanguage
lookupLanguage cfg x
    = find (elem x . languageIdentifiers)
    $ configLanguages cfg

languageFromName :: Config -> Text -> Maybe ConfigLanguage
languageFromName cfg x
    = find ((== x) . languageName)
    $ configLanguages cfg
```
