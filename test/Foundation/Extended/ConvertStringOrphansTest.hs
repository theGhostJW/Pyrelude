
module Foundation.Extended.ConvertStringOrphansTest where

import qualified Data.Bool                           as B
import           Foundation.Extended.Stringy  as S
import           Foundation.Extended as F
import           Test.Extended as T
import           Data.Text
import qualified Prelude as P

hprop_string_to_char_list :: Property
hprop_string_to_char_list = property $ do
                                        str <- forAll $ string (linear 0 1000) unicode
                                        toS (toS str :: P.String) === str

hprop_char_list_to_string :: Property
hprop_char_list_to_string = property $ do
                                        str <- forAll $ charList (linear 0 1000) unicode
                                        toS (toS str :: String) === str

hprop_string_to_text :: Property
hprop_string_to_text = property $ do
                                    str <- forAll $ string (linear 0 1000) unicode
                                    toS (toS str :: Text) === str

hprop_text_to_string :: Property
hprop_text_to_string = property $ do
                                    str <- forAll $ text (linear 0 1000) unicode
                                    toS (toS str :: String) === str