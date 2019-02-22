
module Foundation.Extended.Test where

import qualified Data.Bool                           as B
import           Foundation.Extended as F
import           Test.Extended as T

unit_module_of = "Foundation.Extended.Test" ... moduleOf ''StopSign

--- count ---
unit_count_many = 4 ... count (== 5) [1, 2, 3, 5, 5, 6, 7, 5, 5]
unit_count_last = 1 ... count (== 5) [1, 2, 3, 3, 2, 6, 7, 1, 5]
unit_count_first = 1 ... count (== 5) [5, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_zero = 0 ... count (== 5) [7, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_empty = 0 ... count (== 5) []

--- safeHead ---
unit_safeHead_null = Nothing ... safeHead []
unit_safeHead_populated = Just 7 ... safeHead [7, 6, 4, 3]
unit_safeHead_silgleton = Just 0 ... safeHead [0]

--- safeLast ---
unit_safeLast_null = Nothing ... safeLast []
unit_safeLast_populated = Just 3 ... safeLast [7, 6, 4, 3]
unit_safeLast_silgleton = Just 0 ... safeLast [0]

--- firstDuplicate ---
unit_firstDuplicate_null = Nothing ... firstDuplicate []
unit_firstDuplicate_none = Nothing ... firstDuplicate ([7, 6, 4, 3] :: [Int])
unit_firstDuplicate_silgleton = Just 7 ... firstDuplicate ([7, 6, 4, 3, 7] :: [Int])
unit_firstDuplicate_multiple = Just 9 ... firstDuplicate ([9, 0, 6, 4, 3, 0, 9] :: [Int])
unit_firstDuplicate_big = Just 10000 ... firstDuplicate (10000 : 700 : [0..20000] :: [Int])

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

hprop_truthy_and :: Property
hprop_truthy_and = boolCheck (B.&&) (&&)

hprop_truthy_or :: Property
hprop_truthy_or = boolCheck (B.||)  (||)

hprop_truthy_bool :: Property
hprop_truthy_bool = boolCheckSingle (B.bool 1 2) (F.bool 1 2)

hprop_truthy_not :: Property
hprop_truthy_not = boolCheckSingle B.not F.not

data MyEnum = Hot 
              | Warm
              | Tepid 
              | Cool 
              | Cold 
              | Freezing 
              deriving (Show, Eq, Enum)

unit_enum_list = [Hot, Warm, Tepid, Cool, Cold, Freezing] ... (enumList :: [MyEnum])
unit_enum_list_of_Int = [0..10] ... take 11 (enumList :: [Natural])