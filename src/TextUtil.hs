-- ------ language="Haskell" file="src/TextUtil.hs" project://lit/01-entangled.md#101
module TextUtil where

import Data.Char (isSpace)
import Data.Maybe (isNothing, catMaybes)
-- ------ begin <<import-text>>[0] project://lit/01-entangled.md#44
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end

-- ------ begin <<indent>>[0] project://lit/01-entangled.md#115
indent :: Text -> Text -> Text
indent pre text
    = unlines' $ map indentLine $ lines' text
    where indentLine line
            | line == "" = line
            | otherwise  = pre <> line
-- ------ end
-- ------ begin <<unindent>>[0] project://lit/01-entangled.md#124
unindent :: Text -> Text -> Maybe Text
unindent prefix s
    = unlines' <$> sequence (map unindentLine $ lines' s)
    where unindentLine t
            | T.all isSpace t = Just ""
            | otherwise       = T.stripPrefix prefix t
-- ------ end
-- ------ begin <<unlines>>[0] project://lit/01-entangled.md#51
lines' :: Text -> [Text]
lines' text
    | text == ""               = [""]
    | "\n" `T.isSuffixOf` text = T.lines text <> [""]
    | otherwise                = T.lines text

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ------ end
-- ------ begin <<maybe-unlines>>[0] project://lit/01-entangled.md#70
mLines :: Maybe Text -> [Text]
mLines Nothing = []
mLines (Just text) = lines' text

mUnlines :: [Text] -> Maybe Text
mUnlines [] = Nothing
mUnlines ts = Just $ unlines' ts
-- ------ end
-- ------ begin <<tshow>>[0] project://lit/01-entangled.md#94
tshow :: (Show a) => a -> Text
tshow = T.pack . show
-- ------ end
-- ------ end
