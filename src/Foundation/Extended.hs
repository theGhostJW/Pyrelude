module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , module Path.Extended
  , module PathIO
  , module Foundation.Internal -- exports most of Path.IO
  , StringLike(..)
  , Truthy(..)
) where

import           Data.Either
import qualified Data.Text              as Text
import           Debug.Trace.Extended
import           Foundation
import           Foundation.Compat.Text as Compat
import           Foundation.Internal
import           Path.Extended
import           Path.IO.Extended       as PathIO
import qualified Prelude

  -- fromString clashes with isString typeclass
class StringLike a where
  toStr :: a -> String
  fromStr :: String -> a
  toPreludeStr :: a -> Prelude.String
  toText :: a -> Text.Text

instance StringLike String where
  fromStr = id
  toStr = id
  toPreludeStr = toList
  toText = Compat.toText

instance StringLike Text.Text where
  fromStr = Compat.toText
  toStr = fromText
  toPreludeStr = Text.unpack
  toText = id

instance StringLike Prelude.String where
  fromStr = toList
  toStr = fromList
  toPreludeStr = id
  toText = Text.pack

class Truthy b where
  isTruthy :: b -> Bool

  infixl 1 ?
  (?) :: b -> a -> a -> a
  (?) b a1 a2 = if isTruthy b then a1 else a2

instance Truthy Bool where
  isTruthy  = id
