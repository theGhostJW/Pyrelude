
module PyreludeTest where

import qualified Data.Bool                           as B
import           Pyrelude
import           Pyrelude.Test as T
import qualified Prelude as P

unit_module_of = "PyreludeTest" ... moduleOf ''MyEnum

--- count ---
unit_count_many = 4 ... count (== 5) [1, 2, 3, 5, 5, 6, 7, 5, 5]
unit_count_last = 1 ... count (== 5) [1, 2, 3, 3, 2, 6, 7, 1, 5]
unit_count_first = 1 ... count (== 5) [5, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_zero = 0 ... count (== 5) [7, 2, 3, 3, 2, 6, 7, 1, 9]
unit_count_empty = 0 ... count (== 5) []

--- head ---
unit_head_null = Nothing ... head []
unit_head_populated = Just 7 ... head [7, 6, 4, 3]
unit_head_silgleton = Just 0 ... head [0]

--- last ---
unit_last_null = Nothing ... last []
unit_last_populated = Just 3 ... last [7, 6, 4, 3]
unit_last_silgleton = Just 0 ... last [0]

--- tail ---
unit_tail_null = Nothing ... tail []
unit_tail_populated = Just [6, 4, 3] ... tail [7, 6, 4, 3]
unit_tail_silgleton = Just [] ... tail [0]

--- init ---
unit_init_null = Nothing ... init []
unit_init_populated = Just [7, 6, 4] ... init [7, 6, 4, 3]

{-# ANN unit_init_silgleton ("HLint: ignore Use fmap" :: P.String) #-}
unit_init_silgleton = Just [] ... init [0]

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