-- ------ language="Haskell" file="test/ListStreamSpec.hs" project://lit/a3-megaparsec.md#117
module ListStreamSpec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec
import Data.Void

import ListStream

-- ------ begin <<list-stream-props>>[0] project://lit/a3-megaparsec.md#137
prop_parser :: (Eq a) => Parsec e s a -> s -> a -> Bool
prop_parser p input expected = success (parse p "" input)
    where success (Left err) = False
          success (Right x) = x == expected

parseAny :: (Show a, Ord a, Eq a) => Parsec Void (ListStream a) [a]
parseAny = takeWhileP Nothing (const True)

prop_tokens :: [Int] -> Bool
prop_tokens xs = prop_parser parseAny (ListStream xs) xs
-- ------ end

listStreamSpec :: Spec
listStreamSpec = do
    -- ------ begin <<list-stream-spec>>[0] project://lit/a3-megaparsec.md#150
    describe "Parsing integers" $
        it "takeWhileP yield input" $ do
            property prop_tokens
    -- ------ end
-- ------ end
