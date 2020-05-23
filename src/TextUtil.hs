-- ~\~ language=Haskell filename=src/TextUtil.hs
-- ~\~ begin <<lit/01-entangled.md|src/TextUtil.hs>>[0]
module TextUtil where

import Data.Char (isSpace)
import Data.Maybe (isNothing, catMaybes)
-- ~\~ begin <<lit/01-entangled.md|import-text>>[0]
import RIO (Text)
import qualified RIO.Text as T
-- ~\~ end

-- ~\~ begin <<lit/01-entangled.md|indent>>[0]
indent :: Text -> Text -> Text
indent pre text
    = unlines' $ map indentLine $ lines' text
    where indentLine line
            | line == "" = line
            | otherwise  = pre <> line
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|unindent>>[0]
unindent :: Text -> Text -> Maybe Text
unindent prefix s
    = unlines' <$> sequence (map unindentLine $ lines' s)
    where unindentLine t
            | T.all isSpace t = Just ""
            | otherwise       = T.stripPrefix prefix t
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|unlines>>[0]
lines' :: Text -> [Text]
lines' text
    | text == ""               = [""]
    | "\n" `T.isSuffixOf` text = T.lines text <> [""]
    | otherwise                = T.lines text

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|maybe-unlines>>[0]
mLines :: Maybe Text -> [Text]
mLines Nothing = []
mLines (Just text) = lines' text

mUnlines :: [Text] -> Maybe Text
mUnlines [] = Nothing
mUnlines ts = Just $ unlines' ts
-- ~\~ end
-- ~\~ begin <<lit/01-entangled.md|tshow>>[0]
tshow :: (Show a) => a -> Text
tshow = T.pack . show
-- ~\~ end
-- ~\~ end
