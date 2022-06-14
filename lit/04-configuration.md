# Configuration

The configuration is written in Dhall and has the following schema:

``` {.dhall file=data/config-schema.dhall}
let Comment : Type = < Line : Text | Block : { start : Text, end : Text } >
let Language : Type = { name : Text, identifiers : List Text, comment : Comment }
let LineDirective : Type = { name : Text, format: Text }

<<config-comment-styles>>
<<config-languages>>

let Annotate = < Naked | Standard | Project >

let Syntax : Type =
    { matchCodeStart       : Text
    , extractLanguage      : Text
    , extractFileName      : Text
    , extractReferenceName : Text
    , matchCodeEnd         : Text
    , extractProperty      : Text -> Text }

let defaultSyntax : Syntax =
    { matchCodeStart       = "^[ ]*```[ ]*{[^{}]*}"
    , matchCodeEnd         = "^[ ]*```"
    , extractLanguage      = "^[ ]*```[ ]*{\\.([^{} \t]+)[^{}]*}"
    , extractReferenceName = "^[ ]*```[ ]*{[^{}]*#([^{} \t]*)[^{}]*}"
    , extractFileName      = "^[ ]*```[ ]*{[^{}]*file=([^{} \t]*)[^{}]*}"
    , extractProperty      = \(name : Text) -> "^[ ]*```[ ]*{[^{}]*${name}=([^{} \t]*)[^{}]*}" }

let Config =
    { Type =
        { version   : Text
        , languages : List Language
        , watchList : List Text
        , database  : Optional Text
        , syntax    : Syntax
        , annotate  : Annotate
        , lineDirectives : List LineDirective
        , useLineDirectives : Bool
        , lineNumberSeparator : Text }
    , default =
        { version   = "1.4.0"
        , languages = languages
        , watchList = [] : List Text
        , database  = None Text
        , syntax    = defaultSyntax
        , annotate  = Annotate.Standard
        , lineDirectives = lineDirectives
        , useLineDirectives = False
        , lineNumberSeparator = "#" }
    }

in { Comment   = Comment
   , Language  = Language
   , LineDirective = LineDirective
   , Config    = Config
   , Annotate  = Annotate
   , Syntax    = Syntax
   , comments  = comments
   , languages = languages
   , lineDirectives = lineDirectives
   , defaultSyntax = defaultSyntax
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
    , { name = "Bash",       identifiers = ["bash", "sh"],    comment = comments.hash }
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
    , { name = "<unknown>",  identifiers = [] : List Text,    comment = comments.hash }
    ]

let lineDirectives =
    [ { name = "C",          format = "#line {linenumber} \"{filename}\"" }
    , { name = "C++",        format = "#line {linenumber} \"{filename}\"" }
    , { name = "Haskell",    format = "{{-# LINE {linenumber} \"{filename}\" #-}}" }
    ]
```

## Older versions
We still would like to read older version of the schema.

``` {.haskell file=src/Config/Version_1_0_0.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Version_1_0_0 where

import RIO

import Config.Record
import Format
import Dhall (auto, Decoder, record, field, setFromDistinctList)

<<config-1-0-0-record>>
<<config-1-0-0-decoder>>
```

``` {.haskell file=src/Config/Version_1_2_0.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Version_1_2_0 where

import RIO

import Config.Record hiding (ConfigSyntax, configSyntaxDecoder)
import qualified Config.Version_1_0_0 as Version_1_0_0
import Format

import Dhall (auto, Decoder, record, field, setFromDistinctList )

<<config-1-2-0-record>>
<<config-1-2-0-decoder>>
```

``` {.haskell file=src/Config/Version_1_3_0.hs}
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Config.Version_1_3_0 where

import RIO
import qualified RIO.Text as T

import Config.Record
import qualified Config.Version_1_2_0 as Version_1_2_0
import Format

import Paths_entangled
import Dhall (input, auto, Decoder, record, field, setFromDistinctList )

<<config-1-3-0-record>>
<<config-1-3-0-decoder>>
```

``` {.haskell file=src/Config/Version_1_4_0.hs}
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Config.Version_1_4_0 where

import RIO
import qualified RIO.Text as T

import Config.Record
import qualified Config.Version_1_0_0 as Version_1_0_0
import qualified Config.Version_1_2_0 as Version_1_2_0
import qualified Config.Version_1_3_0 as Version_1_3_0
import Format

import Paths_entangled
import Dhall (input, auto, Decoder, record, field, setFromDistinctList )

<<config-1-4-0-record>>
<<config-1-4-0-decoder>>
```

``` {.haskell file=src/Config/Record.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Record where

import RIO
import qualified RIO.Map as M

<<config-imports>>
<<config-dhall-schema>>
```

## Reading config

``` {.haskell #config-imports}
import Dhall (FromDhall, ToDhall, auto, Decoder, record, list
             , field, constructor, unit, union)

import qualified Format
```

We need to match the Dhall schema with types in Haskell

### Language

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
```

### Line directives

``` {.haskell #config-dhall-schema}
decodeFormatSpec :: Decoder Format.Spec
decodeFormatSpec = fromMaybe [Format.Plain "illegal format spec"] . Format.spec <$> auto

lineDirectivesDecoder :: Decoder (Map Text Format.Spec)
lineDirectivesDecoder = M.fromList <$> list entry
    where entry = record ( pair <$> field "name" auto
                                <*> field "format" decodeFormatSpec )
          pair a b = (a, b)
```

### Annotation method

``` {.haskell #config-dhall-schema}
data AnnotateMethod = AnnotateNaked
                    | AnnotateStandard
                    | AnnotateProject
                    deriving (Show, Eq)

annotateDecoder :: Decoder AnnotateMethod
annotateDecoder = union
        (  ( AnnotateNaked          <$ constructor "Naked" unit )
        <> ( AnnotateStandard       <$ constructor "Standard" unit )
        <> ( AnnotateProject        <$ constructor "Project" unit ) )
```

### Syntax

``` {.haskell #config-dhall-schema}
data ConfigSyntax = ConfigSyntax
    { matchCodeStart       :: Text
    , matchCodeEnd         :: Text
    , extractLanguage      :: Text
    , extractReferenceName :: Text
    , extractFileName      :: Text
    , extractProperty      :: Text -> Text
    }

configSyntaxDecoder :: Decoder ConfigSyntax
configSyntaxDecoder = record
    ( ConfigSyntax <$> field "matchCodeStart" auto
                   <*> field "matchCodeEnd" auto
                   <*> field "extractLanguage" auto
                   <*> field "extractReferenceName" auto
                   <*> field "extractFileName" auto
                   <*> field "extractProperty" auto )
```

### Decoders

#### Version 1.0.0

``` {.haskell #config-1-0-0-record}
data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    } deriving (Show)
```

``` {.haskell #config-1-0-0-decoder}
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
```


#### Version 1.2.0

``` {.haskell #config-1-2-0-record}
data ConfigSyntax = ConfigSyntax
    { matchCodeStart       :: Text
    , matchCodeEnd         :: Text
    , extractLanguage      :: Text
    , extractReferenceName :: Text
    , extractFileName      :: Text
    }

configSyntaxDecoder :: Decoder ConfigSyntax
configSyntaxDecoder = record
    ( ConfigSyntax <$> field "matchCodeStart" auto
                   <*> field "matchCodeEnd" auto
                   <*> field "extractLanguage" auto
                   <*> field "extractReferenceName" auto
                   <*> field "extractFileName" auto )

data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configSyntax    :: ConfigSyntax
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    }

defaultSyntax :: ConfigSyntax
defaultSyntax = ConfigSyntax
    { matchCodeStart       = "^[ ]*```[ ]*{[^{}]*}"
    , matchCodeEnd         = "^[ ]*```"
    , extractLanguage      = "^[ ]*```[ ]*{\\.([^{} \t]+)[^{}]*}"
    , extractReferenceName = "^[ ]*```[ ]*{[^{}]*#([^{} \t]*)[^{}]*}"
    , extractFileName      = "^[ ]*```[ ]*{[^{}]*file=([^{} \t]*)[^{}]*}" }

class ToVersion_1_2_0 a where
    update :: a -> IO Config

instance ToVersion_1_2_0 Config where
    update = return

instance ToVersion_1_2_0 Version_1_0_0.Config where
    update old = do
        return Config
            { configVersion           = Version_1_0_0.configVersion           old
            , configLanguages         = Version_1_0_0.configLanguages         old
            , configWatchList         = Version_1_0_0.configWatchList         old
            , configDatabase          = Version_1_0_0.configDatabase          old
            , configSyntax            = defaultSyntax
            , configAnnotate          = Version_1_0_0.configAnnotate          old
            , configLineDirectives    = Version_1_0_0.configLineDirectives    old
            , configUseLineDirectives = Version_1_0_0.configUseLineDirectives old
            }
```

``` {.haskell #config-1-2-0-decoder}
configDecoder :: Decoder Config
configDecoder = record
    ( Config <$> field "version" auto
             <*> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
             <*> field "syntax" configSyntaxDecoder
             <*> field "annotate" annotateDecoder
             <*> field "lineDirectives" lineDirectivesDecoder
             <*> field "useLineDirectives" auto
    )
```

#### Version 1.3.0

``` {.haskell #config-1-3-0-record}
data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configSyntax    :: ConfigSyntax
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    }

defaultSyntax :: IO ConfigSyntax
defaultSyntax = do
    path <- getDataFileName "data/config-schema.dhall"
    input configSyntaxDecoder $ "(" <> T.pack path <> ").defaultSyntax"

class ToVersion_1_3_0 a where
    update :: a -> IO Config

instance ToVersion_1_3_0 Config where
    update = return

syntax120to130 :: Version_1_2_0.ConfigSyntax -> ConfigSyntax
syntax120to130 Version_1_2_0.ConfigSyntax {..}
    = ConfigSyntax
        { extractProperty      = \name -> "^[ ]*```[ ]*{[^{}]*" <> name <> "=([^{} \t]*)[^{}]*}"
        , .. }

instance Version_1_2_0.ToVersion_1_2_0 a => ToVersion_1_3_0 a where
    update old' = do
        old <- Version_1_2_0.update old'
        return Config
            { configVersion           = Version_1_2_0.configVersion           old
            , configLanguages         = Version_1_2_0.configLanguages         old
            , configWatchList         = Version_1_2_0.configWatchList         old
            , configDatabase          = Version_1_2_0.configDatabase          old
            , configSyntax            = syntax120to130 $ Version_1_2_0.configSyntax old
            , configAnnotate          = Version_1_2_0.configAnnotate          old
            , configLineDirectives    = Version_1_2_0.configLineDirectives    old
            , configUseLineDirectives = Version_1_2_0.configUseLineDirectives old
            }
```

``` {.haskell #config-1-3-0-decoder}
configDecoder :: Decoder Config
configDecoder = record
    ( Config <$> field "version" auto
             <*> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
             <*> field "syntax" configSyntaxDecoder
             <*> field "annotate" annotateDecoder
             <*> field "lineDirectives" lineDirectivesDecoder
             <*> field "useLineDirectives" auto
    )
```

#### Version 1.4.0
A new feature is added to customize project directives.

``` {.haskell #config-1-4-0-record}
data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configSyntax    :: ConfigSyntax
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    , configLineNumberSeparator :: Text
    }

defaultSyntax :: IO ConfigSyntax
defaultSyntax = do
    path <- getDataFileName "data/config-schema.dhall"
    input configSyntaxDecoder $ "(" <> T.pack path <> ").defaultSyntax"

class ToVersion_1_4_0 a where
    update :: a -> IO Config

instance ToVersion_1_4_0 Config where
    update = return

instance ToVersion_1_4_0 Version_1_0_0.Config where
    update old = Version_1_2_0.update old >>= Version_1_3_0.update >>= update

instance ToVersion_1_4_0 Version_1_2_0.Config where
    update old = Version_1_3_0.update old >>= update

instance ToVersion_1_4_0 Version_1_3_0.Config where
    update old = do
        return Config
            { configVersion           = Version_1_3_0.configVersion           old
            , configLanguages         = Version_1_3_0.configLanguages         old
            , configWatchList         = Version_1_3_0.configWatchList         old
            , configDatabase          = Version_1_3_0.configDatabase          old
            , configSyntax            = Version_1_3_0.configSyntax            old
            , configAnnotate          = Version_1_3_0.configAnnotate          old
            , configLineDirectives    = Version_1_3_0.configLineDirectives    old
            , configUseLineDirectives = Version_1_3_0.configUseLineDirectives old
            , configLineNumberSeparator = "#"
            }
```

``` {.haskell #config-1-4-0-decoder}
configDecoder :: Decoder Config
configDecoder = record
    ( Config <$> field "version" auto
             <*> field "languages" (setFromDistinctList configLanguage)
             <*> field "watchList" auto
             <*> field "database" auto
             <*> field "syntax" configSyntaxDecoder
             <*> field "annotate" annotateDecoder
             <*> field "lineDirectives" lineDirectivesDecoder
             <*> field "useLineDirectives" auto
             <*> field "lineNumberSeparator" auto
    )
```


``` {.haskell file=src/Config.hs}
{-# LANGUAGE NoImplicitPrelude #-}
module Config ( module Config
              , module Version_1_4_0
              , module Config.Record ) where

import RIO hiding (void)
import RIO.Directory
import RIO.FilePath
import RIO.List (scanl1, find)
import qualified RIO.Text as T

import Dhall (input, auto)
import System.FilePath.Glob

import qualified Config.Version_1_0_0 as Version_1_0_0
import qualified Config.Version_1_2_0 as Version_1_2_0
import qualified Config.Version_1_3_0 as Version_1_3_0
import qualified Config.Version_1_4_0 as Version_1_4_0
import Config.Version_1_4_0 (update, Config(..))
import Config.Record

import Errors
import Select

<<config-input>>
<<config-reader>>

class HasConfig env where
    config :: Lens' env Config

getDatabasePath :: (MonadIO m, MonadThrow m) => Config -> m FilePath
getDatabasePath cfg = do
    dbPath <- case configDatabase cfg of
        Nothing -> return ":memory:"
        Just db -> return $ T.unpack db
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
    return dbPath

getInputFiles :: (MonadIO m) => Config -> m [FilePath]
getInputFiles cfg = liftIO $ mconcat <$> mapM (glob . T.unpack) (configWatchList cfg)
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

getVersion :: FilePath -> IO Text
getVersion path =
    input auto $ "(" <> T.pack path <> ").entangled.version"

readLocalConfig :: IO Config
readLocalConfig = do
    cfg_path <- findFileAscending "entangled.dhall"
            >>= maybe (throwM $ SystemError "no config found") return
    version <- getVersion cfg_path
    decoder <- select (throwM $ SystemError $ "unrecognized version string '" <> version <> "'")
        [ ( version == "1.0.0", return $ update <=< input Version_1_0_0.configDecoder )
        , ( version == "1.2.0", return $ update <=< input Version_1_2_0.configDecoder )
        , ( version == "1.3.0", return $ update <=< input Version_1_3_0.configDecoder )
        , ( version == "1.4.0", return $ input Version_1_4_0.configDecoder ) ]
    decoder $ "(" <> T.pack cfg_path <> ").entangled"
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
