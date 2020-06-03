-- ~\~ language=Haskell filename=src/Transaction.hs
-- ~\~ begin <<lit/a4-fileio.md|src/Transaction.hs>>[0]
{-# LANGUAGE NoImplicitPrelude,UndecidableInstances #-}
module Transaction where

import RIO
-- ~\~ begin <<lit/a4-fileio.md|transaction-imports>>[0]
-- import qualified Data.Text.Prettyprint.Doc as P
import Console (Doc, group)
import qualified Console
import qualified Data.Text.IO as T.IO
-- ~\~ end

data Transaction m = Transaction
  { action :: Maybe (m ())
  , description :: Doc
  , needConfirm :: Bool }

-- ~\~ begin <<lit/a4-fileio.md|transaction>>[0]
instance (Semigroup (m ())) => Semigroup (Transaction m) where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance (Monoid (m ())) => Monoid (Transaction m) where
    mempty = Transaction mempty mempty False
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|transaction>>[1]
plan :: (Monad m) => m () -> Transaction m
plan action = Transaction (Just action) mempty False

doc :: Doc -> Transaction m
doc x = Transaction Nothing x False

confirm :: Transaction m
confirm = Transaction Nothing mempty True
-- ~\~ end
-- ~\~ begin <<lit/a4-fileio.md|transaction>>[2]
runTransaction :: (MonadIO m) => Maybe Doc -> Transaction m -> m ()
runTransaction (Just h) (Transaction Nothing d _) = liftIO $ Console.putTerminal $ group h d
runTransaction Nothing (Transaction Nothing d _) = liftIO $ Console.putTerminal d
runTransaction h (Transaction (Just x) d c) = do
    liftIO $ Console.putTerminal $ maybe d (`group` d) h
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
