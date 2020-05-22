-- ------ language="Haskell" file="src/Entangled.hs" project://lit/12-main.md
{-# LANGUAGE NoImplicitPrelude #-}
module Entangled where

import RIO
import RIO.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified RIO.Text as T

import qualified Data.Map.Lazy as LM

import FileIO
import Transaction

import Console (msgWrite, msgCreate, msgDelete)
import Paths_entangled
import Config (config, HasConfig, languageFromName)
import Database ( db, HasConnection, queryTargetRef, queryReferenceMap
                , listTargetFiles, insertDocument, insertTargets, stitchDocument
                , deduplicateRefs, updateTarget, listOrphanTargets, clearOrphanTargets )
import Errors (EntangledError (..))

import Comment (headerComment)
import Document (ReferenceName(..))
import Tangle (ExpandedCode, Annotator, expandedCode, parseMarkdown')
import Stitch (untangle)

type FileTransaction env = Transaction (FileIO env)

newtype Entangled env a = Entangled { unEntangled :: WriterT (FileTransaction env) (RIO env) a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadThrow
             , MonadReader env, MonadWriter (FileTransaction env) )

runEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Entangled env a -> m a
runEntangled (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    runFileIO' $ runTransaction w
    return r

instance (HasLogFunc env) => MonadFileIO (Entangled env) where
    readFile path       = readFile' path
    dump text           = dump' text

    writeFile path text = do
        old_content' <- liftRIO $ try $ runFileIO' $ readFile path
        case (old_content' :: Either IOException Text) of
            Right old_content | old_content == text -> return ()
                              | otherwise           -> actionw
            Left  _                                 -> actionc
        where actionw   = tell $ doc (msgWrite path)
                              <> plan (writeFile path text)
              actionc   = tell $ doc (msgCreate path)
                              <> plan (writeFile path text)

    deleteFile path     = tell $ doc (msgDelete path)
                              <> plan (deleteFile path)

data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show)

tangleRef :: ExpandedCode -> Annotator -> ReferenceName -> Entangled env Text
tangleRef codes annotate name =
    case codes LM.!? name of
        Nothing        -> throwM $ TangleError $ "Reference `" <> tshow name <> "` not found."
        Just (Left e)  -> throwM $ TangleError $ tshow e
        Just (Right t) -> return t

tangleFile :: (HasConnection env, HasLogFunc env, HasConfig env)
           => ExpandedCode -> Annotator -> FilePath -> Entangled env Text
tangleFile codes annotate path = do
    cfg <- view config
    (db $ queryTargetRef path) >>= \case
        Nothing              -> throwM $ TangleError $ "Target `" <> T.pack path <> "` not found."
        Just (ref, langName) -> do
            content <- tangleRef codes annotate ref
            case languageFromName cfg langName of
                Nothing -> throwM $ TangleError $ "Language unknown " <> langName
                Just lang -> return $ T.unlines [headerComment lang path, content]

tangle :: (HasConnection env, HasLogFunc env, HasConfig env)
       => TangleQuery -> Annotator -> Entangled env ()
tangle query annotate = do
    cfg <- view config
    refs <- db (queryReferenceMap cfg)
    let codes = expandedCode annotate refs
    case query of
        TangleRef ref   -> dump =<< tangleRef codes annotate (ReferenceName ref)
        TangleFile path -> dump =<< tangleFile codes annotate path
        TangleAll       -> mapM_ (\f -> writeFile f =<< tangleFile codes annotate f) =<< db listTargetFiles

stitch :: (HasConnection env, HasLogFunc env, HasConfig env)
       => FilePath -> Entangled env ()
stitch path = dump =<< db (stitchDocument path)

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump =<< (T.unlines <$> map T.pack <$> db listTargetFiles)

insertSources :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertSources files = do
    logDebug $ display $ "inserting files: " <> tshow files
    mapM_ readDoc files
    where readDoc f = do
            doc <- parseMarkdown' f =<< readFile f
            db (insertDocument f doc)

insertTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertTargets files = do
    logDebug $ display $ "inserting files: " <> tshow files
    mapM_ readTgt files
    where readTgt f = do
            refs <- untangle f =<< readFile f
            db (updateTarget =<< deduplicateRefs refs)

clearOrphans :: (HasConnection env, HasLogFunc env, HasConfig env)
             => Entangled env ()
clearOrphans = do
    files <- db $ do
        r <- listOrphanTargets
        clearOrphanTargets
        return r
    mapM_ deleteFile files

printExampleConfig :: (HasLogFunc env)
                   => Entangled env ()
printExampleConfig = dump =<< readFile =<< liftIO (getDataFileName "data/example-config.dhall")
-- ------ end
