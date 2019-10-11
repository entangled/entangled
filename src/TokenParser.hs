module TokenParser where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (liftM, ap)

data Failure
    = EndOfList
    | ChoiceFail [Failure]
    | CustomFailure Text
    deriving (Eq, Show)

newtype TokenParser t a = TokenParser
    { tokenParser :: [t] -> Either Failure (a, [t]) }

instance Functor (TokenParser t) where
    fmap = liftM

instance Applicative (TokenParser t) where
    pure x = TokenParser (\ts -> Right (x, ts))
    (<*>)  = ap

instance Monad (TokenParser t) where
    p >>= f = TokenParser newParser where
        newParser ts = case (tokenParser p ts) of
            Left msg       -> Left msg
            Right (x, ts') -> tokenParser (f x) ts'

tokenParse :: TokenParser t a -> [t] -> Either Failure a
tokenParse p ts = case tokenParser p ts of
    Left msg     -> Left msg
    Right (x, _) -> Right x

tokenFail :: Failure -> TokenParser t a
tokenFail x = TokenParser (\_ -> Left x)

item :: TokenParser t t
item = TokenParser item'
    where item' []     = Left EndOfList
          item' (t:ts) = Right (t, ts)

try :: TokenParser t a -> TokenParser t (Either Failure a)
try p = TokenParser try' where 
    try' ts = case (tokenParser p ts) of
        Left err       -> Right (Left err, ts)
        Right (x, ts') -> Right (Right x, ts')

pred :: (t -> Bool) -> Failure -> TokenParser t t
pred p err = do
    x <- item
    if p x then tokenFail err else return x

choice :: [TokenParser t a] -> TokenParser t a
choice [] = tokenFail (ChoiceFail [])
choice (p:ps) = do
    x <- try p
    case x of
        Right x' -> return x'
        Left err -> do
            y <- try (choice ps)
            case y of
                Right y'               -> return y'
                Left (ChoiceFail errs) -> tokenFail (ChoiceFail $ err:errs)
                Left err'              -> tokenFail err'

many :: TokenParser t a -> TokenParser t [a]
many p = do
    x <- try p
    case x of
        Left _ -> return []
        Right x' -> do
            xs <- many p
            return (x':xs)

some :: TokenParser t a -> TokenParser t [a]
some p = do
    x <- p
    xs <- many p
    return (x:xs)

manyEndWith :: TokenParser t a -> TokenParser t b -> TokenParser t [a]
manyEndWith pa pb = do
    as <- many pa
    pb
    return as

