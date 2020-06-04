-- ~\~ language=Haskell filename=src/Transaction.hs
-- ~\~ begin <<lit/a4-fileio.md|src/Transaction.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Transaction where

import RIO
import qualified RIO.Text as T
-- ~\~ begin <<lit/a4-fileio.md|transaction-imports>>[0]
-- import qualified Data.Text.Prettyprint.Doc as P
import Console (Doc, group, msgCreate, msgDelete, msgWrite)
import qualified Console
import qualified Data.Text.IO as T.IO
-- ~\~ end

data Description = Message Doc
                 | CreateFile FilePath
                 | WriteFile FilePath
                 | DeleteFile FilePath

showDescriptionHuman :: [Description] -> Doc
showDescriptionHuman = mconcat . map (\case
    Message x    -> x
    CreateFile f -> msgCreate f
    WriteFile f  -> msgWrite f
    DeleteFile f -> msgDelete f)

showDescriptionMachine :: [Description] -> Text
showDescriptionMachine = T.unlines . mapMaybe (\case
    Message _    -> Nothing
    CreateFile f -> Just $ "+ " <> T.pack f
    WriteFile f  -> Just $ "~ " <> T.pack f
    DeleteFile f -> Just $ "- " <> T.pack f)

data Transaction m = Transaction
  { action :: Maybe (m ())
  , description :: [Description]
  , needConfirm :: Bool }

-- ~\~ begin <<lit/a4-fileio.md|transaction>>[0]
instance (Semigroup (m ())) => Semigroup (Transaction m) where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance (Monoid (m ())) => Monoid (Transaction m) where
    mempty = Transaction mempty mempty False
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|transaction>>[1]
plan :: (Monad m) => Description -> m () -> Transaction m
plan d action = Transaction (Just action) [d] False

doc :: Doc -> Transaction m
doc x = Transaction Nothing [Message x] False

confirm :: Transaction m
confirm = Transaction Nothing mempty True
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|transaction>>[2]
testTransaction :: (MonadIO m) => Transaction m -> m Bool
testTransaction (Transaction Nothing _ _)  = return False
testTransaction (Transaction (Just _) d _) = liftIO $ T.IO.putStr (showDescriptionMachine d) >> return True

runTransactionMachine :: (MonadIO m) => Transaction m -> m ()
runTransactionMachine (Transaction Nothing d _) = liftIO $ T.IO.putStr (showDescriptionMachine d)
runTransactionMachine (Transaction (Just x) d _) = do
    liftIO $ T.IO.putStr (showDescriptionMachine d)
    x

runTransaction :: (MonadIO m) => Maybe Doc -> Transaction m -> m ()
runTransaction (Just h) (Transaction Nothing d _) = liftIO $ Console.putTerminal $ group h (showDescriptionHuman d)
runTransaction Nothing (Transaction Nothing d _) = liftIO $ Console.putTerminal (showDescriptionHuman d)
runTransaction h (Transaction (Just x) d c) = do
    liftIO $ Console.putTerminal $ maybe (showDescriptionHuman d) (`group` showDescriptionHuman d) h
    if c then do
        reply <- liftIO $ do
            T.IO.putStr "confirm? (y/n) "
            hFlush stdout
            T.IO.getLine
        liftIO $ T.IO.putStrLn ""
        when (reply == "y") x
    else x
-- ~\~ end
-- ~\~ end
