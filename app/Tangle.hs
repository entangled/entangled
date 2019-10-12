-- ------ language="Haskell" file="app/Tangle.hs"
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import Text.Regex.TDFA
-- ------ begin <<import-megaparsec>>[0]
import Text.MegaParsec (ParsecT)
-- ------ end

-- ------ begin <<code-header-regex>>[0]
codeHeaderRe = "^``` *{(.*)} *$"
-- ------ end
-- ------ begin <<code-footer-regex>>[0]
codeFooterRe = "^```` *$"
-- ------ end

-- ------ begin <<markdown-data>>[0]
data Markdown =
    MarkdownLine Text
    CodeHeader Text [CodeProperty]
    CodeLine Text
    CodeFooter Text
-- ------ end
-- ------ begin <<parse-markdown>>[0]
type Parser = Parsec Void Text

parseLine :: (MonadReader Config m) -> Text -> m Markdown
parseLine = 

parseCodeHeader :: (MonadReader Config m) -> Text -> m (Maybe Markdown)
parseCodeHeader t = do
    re <- asks codeHeaderRe
    return $ do
        (_, _, _, m) <- t =~~ re
        return $ CodeHeader t (getCodeProperties m)

parseCodeFooter :: (MonadReader Config m) -> Text -> m (Maybe Markdown)
parseCodeFooter t = do
    re <- asks codeFooterRe
    return $ if t =~ re then Just (CodeFooter t) else Nothing

parseMarkdown :: (MonadReader Config m) => Text -> m [Markdown]
parseMarkdown t =
-- ------ end
-- ------ end
