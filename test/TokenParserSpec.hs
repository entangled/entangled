module TokenParserSpec where

import Test.Hspec
import Test.QuickCheck
-- import Data.Either (isLeft)

import TokenParser

prop_item :: [Int] -> Bool
prop_item [] = tokenParse item ([] :: [Int]) == Left EndOfList
prop_item (x:xs) = tokenParse item (x:xs) == Right x

prop_many_item :: [Int] -> Bool
prop_many_item xs = tokenParse (many item) xs == Right xs

prop_some_item :: [Int] -> Bool
prop_some_item [] = tokenParse (some item) ([] :: [Int]) == Left EndOfList
prop_some_item xs = tokenParse (some item) xs == Right xs

tokenParserSpec :: Spec
tokenParserSpec = do
    describe "TokenParser" $ do
        it "quick check item" $ quickCheck prop_item
        it "many item" $ quickCheck prop_many_item
        it "some item" $ quickCheck prop_some_item
    
