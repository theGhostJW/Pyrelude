module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , module Path.Extended
  , module Foundation.Internal -- exports most of Path.IO
  , StringLike(..)
  , Truthy(..)
  , Foundation.Extended.writeFile
) where

import           Data.Either
import qualified Data.Text              as Text
import           Debug.Trace.Extended
import           Foundation
import           Foundation.Compat.Text
import           Foundation.Internal
import           Path.Extended
import           Path.IO.Extended       as PathIO
import qualified Prelude

  -- fromString clashes with isString typeclass
class StringLike a where
  toStr :: a -> String
  fromStr :: String -> a
  toPreludeStr :: a -> Prelude.String

instance StringLike String where
  toStr = id
  fromStr = id
  toPreludeStr = toList

instance StringLike Text.Text where
  toStr = fromText
  fromStr = toText
  toPreludeStr = Text.unpack

instance StringLike Prelude.String where
  toStr = fromList
  fromStr = toList
  toPreludeStr = id

class Truthy b where
  isTruthy :: b -> Bool

  infixl 1 ?
  (?) :: b -> a -> a -> a
  (?) b a1 a2 = if isTruthy b then a1 else a2

instance Truthy Bool where
  isTruthy  = id

writeFile :: StringLike s => Path a File -> s -> Prelude.IO ()
writeFile = PathIO.writeFile toPreludeStr
