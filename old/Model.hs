module Model
    ( TangleError(..)
    , toTangleError
    , isCyclicReference
    ) where

data TangleError
    = TangleError String
    | CyclicReference String
    deriving (Eq)

instance Show TangleError where
    show (TangleError msg) = "Error: " <> msg
    show (CyclicReference msg) = "Cyclic reference: " <> msg

isCyclicReference :: Either TangleError a -> Bool
isCyclicReference (Left (CyclicReference _)) = True
isCyclicReference _ = False

toTangleError :: Show e => Either e a -> Either TangleError a
toTangleError (Left err) = Left $ TangleError $ show err
toTangleError (Right x)  = Right x
