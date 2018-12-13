module Model
    ( TangleError(..)
    , toTangleError
    ) where

newtype TangleError = TangleError String
        deriving (Eq, Show)

toTangleError :: Show e => Either e a -> Either TangleError a
toTangleError (Left err) = Left $ TangleError $ show err
toTangleError (Right x)  = Right x
