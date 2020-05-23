-- ~\~ language=Haskell filename=test/ListStreamSpec.hs
-- ~\~ begin <<lit/a3-megaparsec.md|test/ListStreamSpec.hs>>[0]
module ListStreamSpec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec
import Data.Void

import ListStream

-- ~\~ begin <<lit/a3-megaparsec.md|list-stream-props>>[0]
prop_parser :: (Eq a) => Parsec e s a -> s -> a -> Bool
prop_parser p input expected = success (parse p "" input)
    where success (Left err) = False
          success (Right x) = x == expected

parseAny :: (Show a, Ord a, Eq a) => Parsec Void (ListStream a) [a]
parseAny = takeWhileP Nothing (const True)

prop_tokens :: [Int] -> Bool
prop_tokens xs = prop_parser parseAny (ListStream xs) xs
-- ~\~ end

listStreamSpec :: Spec
listStreamSpec = do
    -- ~\~ begin <<lit/a3-megaparsec.md|list-stream-spec>>[0]
    describe "Parsing integers" $
        it "takeWhileP yield input" $ do
            property prop_tokens
    -- ~\~ end
-- ~\~ end
