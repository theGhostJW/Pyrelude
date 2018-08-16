module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , StringLike(..)
  , (?)
  , (!?)
) where

import           Data.Either
import qualified Data.Text              as Text
import           Debug.Trace.Extended
import           Foundation
import           Foundation.Compat.Text
import qualified Prelude

  -- fromString clashes with isString typeclass
class StringLike a where
  toStr :: a -> String
  fromStr :: String -> a

instance StringLike String where
  toStr = id
  fromStr = id

instance StringLike Text.Text where
  toStr = fromText
  fromStr = toText

instance StringLike Prelude.String where
  toStr = fromList
  fromStr = toList

class Truthy  b where
  isTruthy :: b -> Bool

  infixl 1 ?
  (?) :: b -> a -> a -> a
  (?) b a1 a2 = if isTruthy b then a1 else a2

  infixl 1 !?
  (!?) :: b -> a -> a -> a
  (!?) b = (not (isTruthy b) ?)

instance Truthy Bool where
  isTruthy  = id


-- infixl 1 ?
-- (?) :: Bool -> a -> a -> a
-- (?) True x _  = x
-- (?) False _ y = y
--
-- infixl 1 !?
-- (!?) :: Bool -> a -> a -> a
-- (!?) b = (?) (not b)

--print s = Prelude. $ toString s
