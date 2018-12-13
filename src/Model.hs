module Model
    ( TangleError(..)
    , toTangleError
    , isCyclicReference
    ) where

data TangleError
    = TangleError String
    | CyclicReference String
    deriving (Eq, Show)

isCyclicReference :: Either TangleError a -> Bool
isCyclicReference (Left (CyclicReference _)) = True
isCyclicReference _ = False

toTangleError :: Show e => Either e a -> Either TangleError a
toTangleError (Left err) = Left $ TangleError $ show err
toTangleError (Right x)  = Right x
