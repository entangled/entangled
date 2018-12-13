module Test
    ( StateData(..)
    , randomGen
    ) where

import Lens.Micro.Platform
import System.Random

data StateData = StateData
    { _randomGen :: StdGen
    } deriving (Show)

randomGen :: Lens' StateData StdGen
randomGen = lens _randomGen (\ s x -> s { _randomGen = x })

-- reinsert :: (a -> b) -> Lens s s a a -> [Lens b b' a s] -> a -> b'
-- reinsert f a bs s
--     = foldr (&) (s ^. a ^. to f) [b %~ (\x -> s & a .~ x) | b <- bs]

instance RandomGen StateData where
    next s  = (s ^. randomGen ^. to next) 
            & _2 %~ (\ x -> s & randomGen .~ x)
    split s = (s ^. randomGen ^. to split)
            & _1 %~ (\ x -> s & randomGen .~ x)
            & _2 %~ (\ x -> s & randomGen .~ x)
