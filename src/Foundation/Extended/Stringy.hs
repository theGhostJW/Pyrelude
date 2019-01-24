module Foundation.Extended.Stringy (
  Stringy(..)
) where

import qualified Data.Text              as Text
import           Foundation
import qualified Foundation.Compat.Text as C
import qualified Prelude                as P

  -- fromString clashes with isString typeclass
class Stringy a where
  fromStr :: String -> a
  toStr :: a -> String
  toS :: Stringy b => b -> a
  toCharList :: a -> P.String
  toText :: a -> Text.Text

instance Stringy String where
  fromStr = id
  toStr = id
  toS = toStr
  toCharList = toList
  toText = C.toText

instance Stringy Text.Text where
  fromStr = C.toText
  toStr = C.fromText
  toS = toText
  toCharList = Text.unpack
  toText = id

instance Stringy P.String where
  fromStr = toList
  toStr = fromList
  toS = toCharList
  toCharList = id
  toText = Text.pack
