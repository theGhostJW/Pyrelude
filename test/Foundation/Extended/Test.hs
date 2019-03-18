
module Foundation.Extended.Test where

import qualified Data.Bool                           as B
import           Pyrelude
import           Pyrelude.Test as T

unit_module_of = "Foundation.Extended.Test" ... moduleOf ''MyEnum

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

-- Ternary --
unit_bool_ternary_true = chk $ (1 < 2) ? True $ False
unit_bool_ternary_false = chkFalse $ (1 > 2) ? True $ False

data MyEnum = Hot 
              | Warm
              | Tepid 
              | Cool 
              | Cold 
              | Freezing 
              deriving (Show, Eq, Enum)

unit_enum_list = [Hot, Warm, Tepid, Cool, Cold, Freezing] ... (enumList :: [MyEnum])
unit_enum_list_of_Int = [0..10] ... take 11 (enumList :: [Natural])