module Foundation.Extended (
  module Foundation
  , StringLike(..)
) where

import qualified Data.Text              as Text
import           Foundation
import           Foundation.Compat.Text
import qualified Prelude

class StringLike a where
  toString :: a -> String

instance StringLike String where
  toString = id

instance StringLike Text.Text where
  toString = fromText

instance StringLike Prelude.String where
  toString = fromList

--print s = Prelude. $ toString s
