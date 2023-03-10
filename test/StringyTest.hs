
module StringyTest where

import qualified Data.Bool                           as B
import           PyrethrumExtras
import  qualified Prelude as P
import           PyrethrumExtras.Test
import Data.Text (Text)
import BasePrelude (($))


hprop_string_to_text :: Property
hprop_string_to_text = property $ do
                                    str <- forAll $ string (linear 0 1000) unicode
                                    toS (toS str :: Text) === str

hprop_text_to_string :: Property
hprop_text_to_string = property $ do
                                    str <- forAll $ text (linear 0 1000) unicode
                                    toS (toS str :: P.String) === str