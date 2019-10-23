-- ------ language="Haskell" file="test/TextUtilSpec.hs"
module TextUtilSpec where

-- ------ begin <<import-text>>[0]
import qualified Data.Text as T
import Data.Text (Text)
-- ------ end
import Data.Maybe (catMaybes)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text

import TextUtil

propUnlines :: Maybe Text -> Bool
propUnlines t = 
    -- ------ begin <<test-unlines-inverse>>[0]
    t == mUnlines (mLines t)
    -- ------ end

propUnlineLists :: ([Text], [Text]) -> Bool
propUnlineLists (a, b) =
    -- ------ begin <<test-unlines-associative>>[0]
    mUnlines (catMaybes [mUnlines a, mUnlines b]) == mUnlines (a <> b)
    -- ------ end

textUtilSpec :: Spec
textUtilSpec = do
    describe "property check" $ do
        it "mUnlines inverses mLines" $
            property $ propUnlines
        it "mUnlines can be nested (associativity)" $
            property $ propUnlineLists
-- ------ end
