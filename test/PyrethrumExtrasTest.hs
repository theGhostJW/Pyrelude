module PyrethrumExtrasTest where

import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import Data.Bool qualified as B
import Data.Map.Strict qualified as M
import PyrethrumExtras.Test as T
import Data.Text ( Text, init, isInfixOf, tail )
import Prelude qualified as P
import PyrethrumExtras as PE
import BasePrelude
    ( Int,
      Show,
      Eq (..),
      Enum,
      Natural,
      Maybe(..),
      ($),
      Bool(..),
      Ord(..),
      Applicative(..),
      String, (<$>), (.) )
import BasePrelude.DataTypes (Maybe)

--- firstJustf ---

unit_firstJustf_middle = Just 5 ... firstJustf (P.length <$>) ([Nothing, Just "12345", Nothing] :: [Maybe String])

unit_firstJustf_start = Just 5 ... firstJustf (P.length <$>) [Just "12345", Just "1", Nothing]

unit_firstJustf_end = Just 5 ... firstJustf (P.length <$>) [Nothing, Nothing, Just "12345"]

unit_firstJustf_nothing = Nothing ... firstJustf (P.length <$>) [Nothing, Nothing, Nothing]

unit_firstJustf_empty = Nothing ... firstJustf (P.length <$>) []

--- firstJust ---

unit_firstJust_middle = Just "12345" ... firstJust ([Nothing, Just "12345", Nothing] :: [Maybe String])

unit_firstJust_start = Just "12345" ... firstJust [Just "12345", Just "1", Nothing]

unit_firstJust_end = Just "12345" ... firstJust [Nothing, Nothing, Just "12345"]

unit_firstJust_nothing = Nothing ... firstJust [Nothing, Nothing, Nothing]

unit_firstJust_empty = Nothing ... firstJust []

--- moduleOf ---
unit_module_of = "PyrethrumExtrasTest" ... moduleOf ''MyEnum

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

expected :: M.Map Text P.Int =
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

--- firstDuplicate ---
unit_firstDuplicate_null = Nothing ... firstDuplicate []

unit_firstDuplicate_none = Nothing ... firstDuplicate ([7, 6, 4, 3] :: [Int])

unit_firstDuplicate_silgleton = Just 7 ... firstDuplicate ([7, 6, 4, 3, 7] :: [Int])

unit_firstDuplicate_multiple = Just 9 ... firstDuplicate ([9, 0, 6, 4, 3, 0, 9] :: [Int])

unit_firstDuplicate_big = Just 10000 ... firstDuplicate (10000 : 700 : [0 .. 20000] :: [Int])

-- Ternary --

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

unit_enum_list_of_Int = [0 .. 10] ... P.take 11 (enumList :: [Natural])

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
