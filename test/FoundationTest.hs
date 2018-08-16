
module FoundationTest where

import           Foundation.Extended
import           Test.Extended
-- import           Test.Tasty.HUnit.Extended


-- text :: MonadGen m => Range Int -> m Char -> m Text
-- text range =
--   fmap Text.pack . string range


genericStringLike g =  property $ do
                        -- str <- forAll $ debug <$> g (Range.linear 0 100) Gen.ascii
                        str <- forAll $ g (linear 0 100) ascii
                        fromStr (toStr str) === str

hprop_stringLike_prelude_string :: Property
hprop_stringLike_prelude_string = genericStringLike preludeString

hprop_stringLike_text :: Property
hprop_stringLike_text = genericStringLike text

hprop_stringLike_string :: Property
hprop_stringLike_string = genericStringLike string

-- unit_chkEq = chkEq 1 1
--
-- unit_chkContains = chkContains "cool wor" "hello cool world"
