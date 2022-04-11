-- ~\~ language=Haskell filename=src/Config/Version_1_3_0.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config/Version_1_3_0.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Config.Version_1_3_0 where

import RIO
import qualified RIO.Text as T

import Config.Record
import qualified Config.Version_1_2_0 as Version_1_2_0
import Format

import Paths_entangled
import Dhall (input, auto, Decoder, record, field, setFromDistinctList )

-- ~\~ begin <<lit/04-configuration.md|config-1-3-0-record>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-1-3-0-decoder>>[0]
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
