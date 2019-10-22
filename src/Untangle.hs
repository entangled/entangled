-- ------ language="Haskell" file="src/Untangle.hs"
module Untangle where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end

import ListStream (ListStream, parseLine, parseLineNot, tokenLine)

type ReferencePair = (ReferenceId, CodeBlock)

sourceDocument :: ( MonadParsec e (ListStream Text) m
                  , MonadReader Config m )
               => m ReferenceMap
sourceDocument = do
    config <- ask
    (prop, _) <- tokenLine (parseLine topHeader)
    let lang' = getAttribute prop "language" 
            >>= languageFromName config
    lang <- maybe (fail "No valid language found in header.") return lang'
    sourceBlock lang

unindent :: Text -> Text -> Maybe Text
unindent prefix s
    | T.all isSpace s = ""
    | otherwise       = T.stripPrefix prefix s

sourceBlock :: ( MonadParsec e (LineStream Text) m )
            => m ([Text], [ReferencePair])
sourceBlock lang = do
    (ref, beginIndent) <- tokenLine (parseLine $ commented lang beginBlock)
    (lines, refpairs) <- mconcat <$> manyTill 
                (sourceBlock lang <|> sourceLine)
                (tokenLine (parseLine $ commented lang endBlock))
    let unindentedLines = map (unindent beginIndent) lines
    when (any isNothing unindentedLines) $ fail "Indentation error"
    let content = unlines' $ catMaybes unindentedLines
    return ( if referenceCount ref == 0
                 then [(indent beginIndent $ nowebReference $ referenceName ref)]
                 else []
           , (ref, unindent beginIndent $ unlines' content):refpairs )

sourceLine :: ( MonadParsec e (LineStream Text) m )
           => m (Text, [ReferencePair])
sourceLine = do
    x <- anySingle
    return ([x], [])
-- ------ end
