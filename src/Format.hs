{-# LANGUAGE NoImplicitPrelude #-}
module Format where

import RIO
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

