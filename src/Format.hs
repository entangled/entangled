{-# LANGUAGE NoImplicitPrelude #-}
module Format where

import RIO
import qualified RIO.Map as M
import Text.Megaparsec (Parsec, chunk, takeWhile1P, parseMaybe)
import Errors (EntangledError(..))

type Parser = Parsec Void Text
data SpecItem = Plain Text | Variable Text deriving (Show)
type Spec = [SpecItem]

specVariableP :: Parser SpecItem
specVariableP = do
    _ <- chunk "{"
    name <- takeWhile1P Nothing (`notElem` ['{', '}'])
    _ <- chunk "}"
    return $ Variable name

specBraceLeftP :: Parser SpecItem
specBraceLeftP = do
    _ <- chunk "{{"
    return $ Plain "{"

specBraceRightP :: Parser SpecItem
specBraceRightP = do
    _ <- chunk "}}"
    return $ Plain "}"

specPlainTextP :: Parser SpecItem
specPlainTextP = Plain <$> takeWhile1P Nothing (`notElem` ['{', '}'])

specP :: Parser Spec
specP = many (specPlainTextP <|> specBraceLeftP <|> specBraceRightP <|> specVariableP)

spec :: Text -> Maybe Spec
spec = parseMaybe specP

formatM :: (MonadThrow m) => Spec -> Map Text Text -> m Text
formatM s args = mconcat <$> mapM resolve s
    where resolve (Plain t) = return t
          resolve (Variable v) = maybe (throwM $ ConfigError $ "unknown variable '" <> v <> "'")
                                       return (args M.!? v)

formatMaybe :: Spec -> Map Text Text -> Maybe Text
formatMaybe s args = mconcat <$> mapM resolve s
    where resolve (Plain t) = Just t
          resolve (Variable v) = args M.!? v

