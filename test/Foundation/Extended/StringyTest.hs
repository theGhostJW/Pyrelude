
module Foundation.Extended.StringyTest where

import qualified Data.Bool                           as B
import           Foundation.Extended.Stringy  as S
import           Foundation.Extended as F
import           Test.Extended as T

genericStringy :: (Show a1, Integral a, MonadGen m, Stringy a1, Eq a1) => (Range a -> m Char -> Gen a1) -> Property
genericStringy g = property $ do
                        str <- forAll $ g (linear 0 1000) unicode
                        fromStr (toStr str) === str

hprop_stringy_prelude_string :: Property
hprop_stringy_prelude_string = genericStringy charList

hprop_stringy_text :: Property
hprop_stringy_text = genericStringy text

hprop_stringy_string :: Property
hprop_stringy_string = genericStringy string

hprop_stringy_bytestring :: Property
hprop_stringy_bytestring = genericStringy bytes