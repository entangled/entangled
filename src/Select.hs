module Select (select, selectM) where

select :: a -> [(Bool, a)] -> a
select defaultChoice []                     = defaultChoice
select defaultChoice ((True,  x) : options) = x
select defaultChoice ((False, _) : options) = select defaultChoice options

selectM :: (Monad m) => m a -> [(m Bool, m a)] -> m a
selectM defaultChoice [] = defaultChoice
selectM defaultChoice ((pred, x) : options) = do
    q <- pred
    if q then x else selectM defaultChoice options

