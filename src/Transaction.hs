-- ------ language="Haskell" file="src/Transaction.hs" project://src/Transaction.hs#2
{-# LANGUAGE UndecidableInstances #-}
module Transaction where

-- ------ begin <<transaction-imports>>[0] project://src/Transaction.hs#3
import RIO (LogLevel)
import qualified Data.Text.Prettyprint.Doc as P
import Console (Doc)
import qualified Console
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as T.IO
import System.IO (stdout, hFlush)
import Control.Monad (when)
-- ------ end

data Transaction m = Transaction
  { action :: Maybe (m ())
  , description :: Doc
  , needConfirm :: Bool }

-- ------ begin <<transaction>>[0] project://src/Transaction.hs#5
instance (Semigroup (m ())) => Semigroup (Transaction m) where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance (Monoid (m ())) => Monoid (Transaction m) where
    mempty = Transaction mempty mempty False
-- ------ end
-- ------ begin <<transaction>>[1] project://src/Transaction.hs#7
plan :: (Monad m) => m () -> Transaction m
plan action = Transaction (Just action) mempty False

doc :: Doc -> Transaction m
doc x = Transaction Nothing x False

msg :: P.Pretty a => LogLevel -> a -> Transaction m
msg level content = Transaction Nothing (Console.msg level content) False

confirm :: Transaction m
confirm = Transaction Nothing mempty True
-- ------ end
-- ------ begin <<transaction>>[2] project://src/Transaction.hs#9
runTransaction :: (MonadIO m) => Transaction m -> m ()
runTransaction (Transaction Nothing d _) = liftIO $ Console.putTerminal d
runTransaction (Transaction (Just x) d c) = do
    liftIO $ Console.putTerminal d
    if c then do
        reply <- liftIO $ do
            T.IO.putStr "confirm? (y/n) "
            hFlush stdout
            getLine
        liftIO $ T.IO.putStrLn ""
        when (reply == "y") x
    else x
-- ------ end
-- ------ end
