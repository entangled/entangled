-- ~\~ language=Haskell filename=src/Config/Record.hs
-- ~\~ begin <<lit/04-configuration.md|src/Config/Record.hs>>[init]
{-# LANGUAGE NoImplicitPrelude #-}
module Config.Record where

import RIO
import qualified RIO.Map as M

-- ~\~ begin <<lit/04-configuration.md|config-imports>>[init]
import Dhall (FromDhall, ToDhall, auto, Decoder, record, list
             , field, constructor, unit, union)

import qualified Format
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-dhall-schema>>[init]
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
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-dhall-schema>>[1]
decodeFormatSpec :: Decoder Format.Spec
decodeFormatSpec = fromMaybe [Format.Plain "illegal format spec"] . Format.spec <$> auto

lineDirectivesDecoder :: Decoder (Map Text Format.Spec)
lineDirectivesDecoder = M.fromList <$> list entry
    where entry = record ( pair <$> field "name" auto
                                <*> field "format" decodeFormatSpec )
          pair a b = (a, b)
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-dhall-schema>>[2]
data AnnotateMethod = AnnotateNaked
                    | AnnotateStandard
                    | AnnotateProject
                    deriving (Show, Eq)

annotateDecoder :: Decoder AnnotateMethod
annotateDecoder = union
        (  ( AnnotateNaked          <$ constructor "Naked" unit )
        <> ( AnnotateStandard       <$ constructor "Standard" unit )
        <> ( AnnotateProject        <$ constructor "Project" unit ) )
-- ~\~ end
-- ~\~ begin <<lit/04-configuration.md|config-dhall-schema>>[3]
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
-- ~\~ end
-- ~\~ end
