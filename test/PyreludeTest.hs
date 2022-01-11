module PyreludeTest where

import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import Data.Bool qualified as B
import Data.Map.Strict qualified as M
import Pyrelude
import Pyrelude.Test as T
import Prelude qualified as P

--- firstJustf ---

unit_firstJustf_middle = Just 5 ... firstJustf (P.length <$>) ([Nothing, Just "12345", Nothing] :: [Maybe P.String])

unit_firstJustf_start = Just 5 ... firstJustf (P.length <$>) [Just "12345", Just "1", Nothing]

unit_firstJustf_end = Just 5 ... firstJustf (P.length <$>) [Nothing, Nothing, Just "12345"]

unit_firstJustf_nothing = Nothing ... firstJustf (P.length <$>) [Nothing, Nothing, Nothing]

unit_firstJustf_empty = Nothing ... firstJustf (P.length <$>) []

--- firstJust ---

unit_firstJust_middle = Just "12345" ... firstJust ([Nothing, Just "12345", Nothing] :: [Maybe P.String])

unit_firstJust_start = Just "12345" ... firstJust [Just "12345", Just "1", Nothing]

unit_firstJust_end = Just "12345" ... firstJust [Nothing, Nothing, Just "12345"]

unit_firstJust_nothing = Nothing ... firstJust [Nothing, Nothing, Nothing]

unit_firstJust_empty = Nothing ... firstJust []

--- moduleOf ---
unit_module_of = "PyreludeTest" ... moduleOf ''MyEnum

--- countValues ---
baseMap :: M.Map Int Text =
  M.fromList
    [ (1, "Hi"),
      (2, "Ho"),
      (3, "Hii"),
      (4, "Ho"),
      (5, "Ho"),
      (6, "Ho"),
      (7, "Hii")
    ]

expected :: M.Map Text Int =
  M.fromList
    [ ("Hi", 1),
      ("Ho", 4),
      ("Hii", 2)
    ]

unit_countValues = expected ... countValues baseMap

unit_countValues_empty = M.empty ... countValues M.empty

--- txtPretty ---
unit_txtPretty = chk . isInfixOf "Hi" $ txtPretty baseMap

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
a :: Maybe [Int]
a = last []

unit_last_null = Nothing ... last []

unit_last_populated = Just 3 ... last [7, 6, 4, 3]

unit_last_silgleton = Just 0 ... last [0]

--- tail ---
unit_tail_null = Nothing ... tail []

unit_tail_populated = Just [6, 4, 3] ... tail [7, 6, 4, 3]

unit_tail_silgleton = Just [] ... tail [0]

--- init ---
unit_init_null = Nothing ... init ([] :: [Int])

unit_init_populated = Just [7, 6, 4] ... init [7, 6, 4, 3]

{-# ANN unit_init_silgleton ("HLint: ignore Use fmap" :: Text) #-}
unit_init_silgleton = Just [] ... init [0]

--- firstDuplicate ---
unit_firstDuplicate_null = Nothing ... firstDuplicate []

unit_firstDuplicate_none = Nothing ... firstDuplicate ([7, 6, 4, 3] :: [Int])

unit_firstDuplicate_silgleton = Just 7 ... firstDuplicate ([7, 6, 4, 3, 7] :: [Int])

unit_firstDuplicate_multiple = Just 9 ... firstDuplicate ([9, 0, 6, 4, 3, 0, 9] :: [Int])

unit_firstDuplicate_big = Just 10000 ... firstDuplicate (10000 : 700 : [0 .. 20000] :: [Int])

-- Ternary --
unit_bool_flipped_ternary_true = chk $ (1 < 2) /? False $ True

unit_bool_filpped_ternary_false = chkFalse $ (1 > 2) /? False $ True

unit_bool_ternary_true = chk $ (1 < 2) ? True $ False

unit_bool_ternary_false = chkFalse $ (1 > 2) ? True $ False

data MyEnum
  = Hot
  | Warm
  | Tepid
  | Cool
  | Cold
  | Freezing
  deriving (Show, Eq, Enum)

unit_enum_list = [Hot, Warm, Tepid, Cool, Cold, Freezing] ... (enumList :: [MyEnum])

unit_enum_list_of_Int = [0 .. 10] ... take 11 (enumList :: [Natural])

logShowable :: (MonadWriter [Text] m, Show a) => a -> m a
logShowable x = writer (x, ["Initialised With: " <> txt x])

unlessTest :: Maybe Text -> [Text]
unlessTest m = execWriter $ do
  unlessJust m $
    tell ["It's Nothing"]
  pure ()

unit_unlessJust_empty = ["It's Nothing"] ... unlessTest Nothing

unit_unlessJust_just = [] ... unlessTest $ Just "something"

-- debug --
data Address = Address
  { line1 :: Text,
    postCode :: Text
  }
  deriving (Show)

data Person = Person
  { name :: Text,
    surname :: Text,
    address :: Address
  }
  deriving (Show)

_debug = debug $ Person "John" "Smith" $ Address "22 vernon St" "3123"
