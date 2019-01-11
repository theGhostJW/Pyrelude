module Foundation.Extended.Stringy (
  Stringy(..)
) where

import qualified Data.Text              as Text
import           Foundation
import           Foundation.Compat.Text as C
import qualified Prelude                as P

  -- fromString clashes with isString typeclass
class Stringy a where
  toStr :: a -> String
  fromStr :: String -> a
  toCharList :: a -> P.String
  toText :: a -> Text.Text

instance Stringy String where
  fromStr = id
  toStr = id
  toCharList = toList
  toText = C.toText

instance Stringy Text.Text where
  fromStr = C.toText
  toStr = fromText
  toCharList = Text.unpack
  toText = id

instance Stringy P.String where
  fromStr = toList
  toStr = fromList
  toCharList = id
  toText = Text.pack
