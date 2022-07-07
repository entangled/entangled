-- ~\~ language=Haskell filename=src/Config/Version_1_0_0.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config/Version_1_0_0.hs>>[init]
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Version_1_0_0 where

import RIO

import Config.Record
import Format
import Dhall (auto, Decoder, record, field, setFromDistinctList)

-- ~\~ begin <<lit/04-configuration.md|config-1-0-0-record>>[init]
data Config = Config
    { configVersion   :: Text
    , configLanguages :: Set ConfigLanguage
    , configWatchList :: [Text]
    , configDatabase  :: Maybe Text
    , configAnnotate  :: AnnotateMethod
    , configLineDirectives :: Map Text Format.Spec
    , configUseLineDirectives :: Bool
    } deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-1-0-0-decoder>>[init]
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
-- ~\~ end
-- ~\~ end
