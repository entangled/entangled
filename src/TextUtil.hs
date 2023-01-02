-- ~\~ language=Haskell filename=src/TextUtil.hs
-- ~\~ begin <<lit/a6-text-utils.md|src/TextUtil.hs>>[init]
{-# LANGUAGE NoImplicitPrelude #-}
module TextUtil where

import RIO
import qualified RIO.Text as T

import Data.Char (isSpace)

-- ~\~ begin <<lit/a6-text-utils.md|indent>>[init]
indent :: Text -> Text -> Text
indent pre text
    = unlines' $ map indentLine $ lines' text
    where indentLine line
            | line == "" = line
            | otherwise  = pre <> line
-- ~\~ end
-- ~\~ begin <<lit/a6-text-utils.md|unindent>>[init]
unindent :: Text -> Text -> Maybe Text
unindent prefix s
    = unlines' <$> mapM unindentLine (lines' s)
    where unindentLine t
            | T.all isSpace t = Just ""
            | otherwise       = T.stripPrefix prefix t
-- ~\~ end
-- ~\~ begin <<lit/a6-text-utils.md|unlines>>[init]
lines' :: Text -> [Text]
lines' text
    | text == ""               = [""]
    | "\n" `T.isSuffixOf` text = T.lines text <> [""]
    | otherwise                = T.lines text

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ~\~ end
-- ~\~ begin <<lit/a6-text-utils.md|maybe-unlines>>[init]
mLines :: Maybe Text -> [Text]
mLines Nothing = []
mLines (Just text) = lines' text

mUnlines :: [Text] -> Maybe Text
mUnlines [] = Nothing
mUnlines ts = Just $ unlines' ts
-- ~\~ end
-- ~\~ end
