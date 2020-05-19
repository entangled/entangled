-- ------ language="Haskell" file="src/Transaction.hs" project://lit/a4-fileio.md
module Transaction where

-- ------ begin <<transaction-imports>>[0] project://lit/a4-fileio.md
import qualified Data.Text.Prettyprint.Doc as P
import Console (Doc)
import qualified Console
import Control.Monad.Logger (LogLevel)
import qualified Data.Text.IO as T.IO
import System.IO (stdout, hFlush)
import Control.Monad (when)
-- ------ end

data Transaction = Transaction
  { action :: Maybe (IO ())
  , description :: Doc
  , needConfirm :: Bool }

-- ------ begin <<transaction>>[0] project://lit/a4-fileio.md
instance Semigroup Transaction where
    (Transaction al dl cl) <> (Transaction ar dr cr)
      = Transaction (al <> ar) (dl <> dr) (cl || cr)

instance Monoid Transaction where
    mempty = Transaction mempty mempty False
-- ------ end
-- ------ begin <<transaction>>[1] project://lit/a4-fileio.md
plan :: IO () -> Transaction
plan action = Transaction (Just action) mempty False

doc :: Doc -> Transaction
doc x = Transaction Nothing x False

msg :: P.Pretty a => LogLevel -> a -> Transaction
msg level doc = Transaction Nothing (Console.msg level doc) False

confirm :: Transaction
confirm = Transaction mempty mempty True
-- ------ end
-- ------ begin <<transaction>>[2] project://lit/a4-fileio.md
runTransaction :: Transaction -> IO ()
runTransaction (Transaction Nothing d _) = Console.putTerminal d
runTransaction (Transaction (Just x) d c) = do
    Console.putTerminal d
    if c then do
        T.IO.putStr "confirm? (y/n) "
        hFlush stdout
        reply <- getLine
        T.IO.putStrLn ""
        when (reply == "y") x
    else x
-- ------ end
-- ------ end
