
module FoundationTest where

import qualified Data.Bool                           as B
import           Foundation.Extended as F
import           Test.Extended as T

genericStringLike g = property $ do
                        str <- forAll $ g (linear 0 100) ascii
                        fromStr (toStr str) === str

hprop_stringLike_prelude_string :: Property
hprop_stringLike_prelude_string = genericStringLike preludeString

hprop_stringLike_text :: Property
hprop_stringLike_text = genericStringLike text

hprop_stringLike_string :: Property
hprop_stringLike_string = genericStringLike string

unit_module_of = chkEq "FoundationTest" $ moduleOf ''StopSign

--- Count ---
unit_count_many = chkEq 4 $ count (== 5) [1, 2, 3, 5, 5, 6, 7, 5, 5]
unit_count_last = chkEq 1 $ count (== 5) [1, 2, 3, 3, 2, 6, 7, 1, 5]
unit_count_first = chkEq 1 $ count (== 5) [5, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_zero = chkEq 0 $ count (== 5) [7, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_empty = chkEq 0 $ count (== 5) []

-- Truthy --
unit_bool_ternary_true = chk $ (1 < 2) ? True $ False
unit_bool_ternary_false = chkFalse $ (1 > 2) ? True $ False

data StopSign = Go | Stop deriving Eq

instance Truthy StopSign where
  isTruthy  = (Go ==)

unit_ternary_custom_true = chk $ Go ? True $ False
unit_ternary_custom_false = chkFalse $ Stop ? True $ False

boolCheck ft fb = property $ do
                         b1 <- forAll T.bool
                         b2 <- forAll T.bool
                         ft b1 b2 === fb b1 b2

boolCheckSingle ft fb = property $ do
                         b <- forAll T.bool
                         ft b === fb b

hprop_truty_and :: Property
hprop_truty_and = boolCheck (B.&&) (&&)

hprop_truty_or :: Property
hprop_truty_or = boolCheck (B.||)  (||)

hprop_truty_bool :: Property
hprop_truty_bool = boolCheckSingle (B.bool 1 2) (F.bool 1 2)

hprop_truty_not :: Property
hprop_truty_not = boolCheckSingle B.not F.not
