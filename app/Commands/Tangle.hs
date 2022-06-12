module Commands.Tangle where

import RIO
import qualified RIO.Text as T
import qualified Data.Map.Lazy as LM
import Options.Applicative (Parser, helper, (<**>), strOption, flag', switch, long, short, help, metavar)

import qualified Commands.Common as Common
import Comment (headerComment)
import Config (HasConfig, Config(..), config, AnnotateMethod(..), languageFromName)
import Database 
    ( HasConnection, db, listTargetFiles, queryTargetRef, queryCodeAttr, queryReferenceMap )
import Entangled (Entangled)
import Errors (EntangledError(..))
import Tangle (ExpandedCode, Annotator, expandedCode, selectAnnotator)
import Document (ReferenceName(..))
import FileIO (dump, writeFile)

data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show, Eq)

takeLines :: Text -> Int -> [Text]
takeLines txt n = take n $ drop 1 $ T.lines txt

dropLines :: Text -> Int -> [Text]
dropLines txt n = take 1 lines_ <> drop (n+1) lines_
    where lines_ = T.lines txt

toInt :: Text -> Maybe Int
toInt = readMaybe . T.unpack

tangleRef :: (HasLogFunc env, HasConfig env)
    => ExpandedCode (Entangled env) -> ReferenceName -> Entangled env Text
tangleRef codes name =
    case codes LM.!? name of
        Nothing        -> throwM $ TangleError $ "Reference `" <> tshow name <> "` not found."
        Just t         -> t

tangleFile :: (HasConnection env, HasLogFunc env, HasConfig env)
           => ExpandedCode (Entangled env) -> FilePath -> Entangled env Text
tangleFile codes path = do
    cfg <- view config
    db (queryTargetRef path) >>= \case
        Nothing              -> throwM $ TangleError $ "Target `" <> T.pack path <> "` not found."
        Just (ref, langName) -> do
            content <- tangleRef codes ref
            headerLen <- db (queryCodeAttr ref "header")
            case languageFromName cfg langName of
                Nothing -> throwM $ TangleError $ "Language unknown " <> langName
                Just lang -> return $ maybe (T.unlines [headerComment lang path, content])
                                            (\n -> T.unlines $ takeLines content n <> [headerComment lang path] <> dropLines content n)
                                            (toInt =<< headerLen)

tangle :: (HasConnection env, HasLogFunc env, HasConfig env)
       => TangleQuery -> Annotator (Entangled env) -> Entangled env ()
tangle query annotate = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let codes = expandedCode annotate refs
    case query of
        TangleRef ref   -> dump =<< tangleRef codes (ReferenceName ref)
        TangleFile path -> dump =<< tangleFile codes path
        TangleAll       -> mapM_ (\f -> writeFile f =<< tangleFile codes f) =<< db listTargetFiles

data Args = Args
    { query :: TangleQuery
    , decorate :: Bool
    } deriving (Show, Eq)

parseArgs :: Parser Args
parseArgs = Args
    <$> (   (TangleFile <$> strOption ( long "file" <> short 'f'
                                      <> metavar "TARGET" <> help "file target" ))
        <|> (TangleRef  <$> strOption ( long "ref"  <> short 'r'
                                      <> metavar "TARGET" <> help "reference target" ))
        <|> flag' TangleAll (long "all" <> short 'a' <> help "tangle all and write to disk" ))
    <*> switch (long "decorate" <> short 'd' <> help "Decorate with stitching comments.")
    <**> helper

run :: Args -> Entangled Common.Env ()
run Args { .. } = do
    cfg <- view config
    tangle query (
        if decorate
            then selectAnnotator cfg
            else selectAnnotator (cfg {configAnnotate = AnnotateNaked}))

