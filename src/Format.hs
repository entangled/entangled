{-# LANGUAGE NoImplicitPrelude #-}
module Format where

import RIO
import qualified RIO.Map as M
import Text.Megaparsec (Parsec, chunk, takeWhile1P)
import Errors (EntangledError(..))

type Parser = Parsec Void Text
data SpecItem = Plain Text | Variable Text
type Spec = [SpecItem]

specVariableP :: Parser SpecItem
specVariableP = do
    _ <- chunk "{"
    name <- takeWhile1P Nothing (`notElem` ['{', '}'])
    _ <- chunk "}"
    return $ Variable name

specBraceLeftP :: Parser SpecItem
specBraceLeftP = Plain <$> chunk "{{"

specBraceRightP :: Parser SpecItem
specBraceRightP = Plain <$> chunk "}}"

specPlainTextP :: Parser SpecItem
specPlainTextP = Plain <$> takeWhile1P Nothing (`notElem` ['{', '}'])

specP :: Parser Spec
specP = many (specPlainTextP <|> specBraceLeftP <|> specBraceRightP <|> specVariableP)

formatM :: (MonadThrow m) => Spec -> Map Text Text -> m Text
formatM spec args = mconcat <$> mapM resolve spec
    where resolve (Plain t) = return t
          resolve (Variable v) = maybe (throwM $ ConfigError $ "unknown variable '" <> v <> "'")
                                       return (args M.!? v)

