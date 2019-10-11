-- ------ language="Haskell" file="app/Tangle.hs"
-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
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
parseMarkdown :: (MonadReader Config m) => Text -> [Markdown]
parseMarkdown t =
-- ------ end
-- ------ end
