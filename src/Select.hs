module Select (select) where

select :: a -> [(Bool, a)] -> a
select defaultChoice []                     = defaultChoice
select defaultChoice ((True,  x) : options) = x
select defaultChoice ((False, _) : options) = select defaultChoice options

