-- ~\~ language=Haskell filename=src/Config/Version_1_2_0.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config/Version_1_2_0.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Version_1_2_0 where

import RIO

import Config.Record hiding (ConfigSyntax, configSyntaxDecoder)
import qualified Config.Version_1_0_0 as Version_1_0_0
import Format

import Dhall (auto, Decoder, record, field, setFromDistinctList )

-- ~\~ begin <<lit/04-configuration.md|config-1-2-0-record>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-1-2-0-decoder>>[0]
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
-- ~\~ end
-- ~\~ end
