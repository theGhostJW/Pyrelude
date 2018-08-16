
module FoundationTest where

import           Foundation.Extended
import           Test.Extended

genericStringLike g = property $ do
                        str <- forAll $ g (linear 0 100) ascii
                        fromStr (toStr str) === str

hprop_stringLike_prelude_string :: Property
hprop_stringLike_prelude_string = genericStringLike preludeString

hprop_stringLike_text :: Property
hprop_stringLike_text = genericStringLike text

hprop_stringLike_string :: Property
hprop_stringLike_string = genericStringLike string

unit_bool_ternary_true = chk $ (1 < 2) ? True $ False
unit_bool_ternary_false = chkFalse $ (1 > 2) ? True $ False

data StopSign = Go | Stop deriving Eq

instance Truthy StopSign where
  isTruthy  = (Go ==)

unit_ternary_custom_true = chk $ Go ? True $ False
unit_ternary_custom_false = chkFalse $ Stop ? True $ False
