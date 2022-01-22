-- ~\~ language=Haskell filename=src/Entangled.hs
-- ~\~ begin <<lit/12-main.md|src/Entangled.hs>>[0]
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Entangled where

import RIO
import RIO.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified RIO.Text as T

import qualified Data.Map.Lazy as LM
import Control.Monad.Except ( MonadError(..) )

import FileIO
import Transaction

import Console (Doc, timeStamp)
import Paths_entangled
import Config (config, HasConfig, languageFromName)
import Database ( db, HasConnection, queryTargetRef, queryReferenceMap
                , listTargetFiles, insertDocument, stitchDocument, listSourceFiles
                , deduplicateRefs, updateTarget, listOrphanTargets, clearOrphanTargets
                , queryCodeAttr )
import Errors (EntangledError (..))

import Comment (headerComment)
import Document (ReferenceName(..))
import Tangle (ExpandedCode, Annotator, expandedCode, parseMarkdown')
import Stitch (untangle)

type FileTransaction env = Transaction (FileIO env)

newtype Entangled env a = Entangled { unEntangled :: WriterT (FileTransaction env) (RIO env) a }
    deriving ( Applicative, Functor, Monad, MonadIO, MonadThrow
             , MonadReader env, MonadWriter (FileTransaction env) )

instance MonadError EntangledError (Entangled env) where
    throwError = throwM
    catchError x _ = x

testEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
              => Entangled env a -> m Bool
testEntangled (Entangled x) = do
    e <- ask
    (_, w) <- runRIO e (runWriterT x)
    runFileIO' $ testTransaction w

runEntangled :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Bool -> Maybe Doc -> Entangled env a -> m a
runEntangled True  _ = runEntangledMachine
runEntangled False h = runEntangledHuman h

runEntangledMachine :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Entangled env a -> m a
runEntangledMachine (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    runFileIO' $ runTransactionMachine w
    return r

runEntangledHuman :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Maybe Doc -> Entangled env a -> m a
runEntangledHuman h (Entangled x) = do
    e <- ask
    (r, w) <- runRIO e (runWriterT x)
    ts <- timeStamp
    runFileIO' $ runTransaction (h >>= (\h' -> Just $ ts <> " " <> h')) w
    return r

instance (HasLogFunc env) => MonadFileIO (Entangled env) where
    readFile = readFile'
    dump = dump'

    writeFile path text = do
        old_content' <- liftRIO $ try $ runFileIO' $ readFile path
        case (old_content' :: Either IOException Text) of
            Right old_content | old_content == text -> return ()
                              | otherwise           -> actionw
            Left  _                                 -> actionc
        where actionw   = tell $ plan (WriteFile path) (writeFile path text)
              actionc   = tell $ plan (CreateFile path) (writeFile path text)

    deleteFile path     = tell $ plan (DeleteFile path) (deleteFile path)

data TangleQuery = TangleFile FilePath | TangleRef Text | TangleAll deriving (Show, Eq)

tangleRef :: (HasLogFunc env, HasConfig env)
    => ExpandedCode (Entangled env) -> ReferenceName -> Entangled env Text
tangleRef codes name =
    case codes LM.!? name of
        Nothing        -> throwM $ TangleError $ "Reference `" <> tshow name <> "` not found."
        Just t         -> t

toInt :: Text -> Maybe Int
toInt = readMaybe . T.unpack

takeLines :: Text -> Int -> [Text]
takeLines txt n = take n $ drop 1 $ T.lines txt

dropLines :: Text -> Int -> [Text]
dropLines txt n = take 1 lines_ <> drop (n+1) lines_
    where lines_ = T.lines txt

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

data StitchQuery = StitchFile FilePath | StitchAll

stitchFile :: (HasConnection env, HasLogFunc env, HasConfig env)
       => FilePath -> Entangled env Text
stitchFile path = db (stitchDocument path)

stitch :: (HasConnection env, HasLogFunc env, HasConfig env)
       => StitchQuery -> Entangled env ()
stitch (StitchFile path) = dump =<< stitchFile path
stitch StitchAll = mapM_ (\f -> writeFile f =<< stitchFile f) =<< db listSourceFiles

listTargets :: (HasConnection env, HasLogFunc env, HasConfig env)
            => Entangled env ()
listTargets = dump . T.unlines . map T.pack =<< db listTargetFiles

insertSources :: (HasConnection env, HasLogFunc env, HasConfig env)
              => [FilePath] -> Entangled env ()
insertSources files = do
    logDebug $ display $ "inserting files: " <> tshow files
    mapM_ readDoc files
    where readDoc f = do
            document <- parseMarkdown' f =<< readFile f
            db (insertDocument f document)

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
-- ~\~ end
