-- ------ language="Haskell" file="src/TextUtil.hs"
module TextUtil where

import Data.Char (isSpace)
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end

-- ------ begin <<indent>>[0]
indent :: Text -> Text -> Text
indent pre text
    = unlines' 
    $ map indentLine
    $ T.lines text
    where indentLine line
            | line == "" = line
            | otherwise  = pre <> line
-- ------ end
-- ------ begin <<unindent>>[0]
unindent :: Text -> Text -> Maybe Text
unindent prefix s
    | T.all isSpace s = Just ""
    | otherwise       = T.stripPrefix prefix s
-- ------ end
-- ------ begin <<unlines>>[0]
lines' :: Text -> [Text]
lines' text
    | text == ""               = [""]
    | "\n" `T.isSuffixOf` text = T.lines text <> [""]
    | otherwise                = T.lines text

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"
-- ------ end
-- ------ begin <<maybe-unlines>>[0]
mLines :: Maybe Text -> [Text]
mLines Nothing = []
mLines (Just text) = lines' text

mUnlines :: [Text] -> Maybe Text
mUnlines [] = Nothing
mUnlines ts = Just $ unlines' ts
-- ------ end
-- ------ begin <<tshow>>[0]
tshow :: (Show a) => a -> Text
tshow = T.pack . show
-- ------ end
-- ------ end
