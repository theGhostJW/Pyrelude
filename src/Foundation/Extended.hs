module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , module Path.Extended
  , module PathIO
  , module Foundation.Internal -- exports most of Path.IO
  , module THEx
  , StringLike(..)
  , Truthy(..)
  , count
) where

import qualified Data.Bool                           as B
import           Data.Either
import qualified Data.Text                           as Text
import           Debug.Trace.Extended
import           Foundation                          hiding (not, (&&), (||))
import           Foundation.Collection
import           Foundation.Compat.Text              as Compat
import           Foundation.Internal
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Path.IO.Extended                    as PathIO
import qualified Prelude                             as P

  -- fromString clashes with isString typeclass
class StringLike a where
  toStr :: a -> String
  fromStr :: String -> a
  toPreludeStr :: a -> P.String
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

instance StringLike P.String where
  fromStr = toList
  toStr = fromList
  toPreludeStr = id
  toText = Text.pack

-- redeclare to complement truthy
{-# ANN otherwise "HLint: ignore" #-}
otherwise :: Bool
otherwise = True


class Truthy b where
  isTruthy :: b -> Bool

  infixl 1 ?
  (?) :: b -> a -> a -> a
  (?) b a1 a2 = if isTruthy b then a1 else a2

  -- and
  infixr 3 &&
  (&&) :: b -> b -> Bool
  (&&) a b = isTruthy a B.&& isTruthy b

  -- or
  infixr 2 ||
  (||) :: b -> b -> Bool
  (||) a b = isTruthy a B.|| isTruthy b

  -- not
  not :: b -> Bool
  not = B.not . isTruthy

  -- reverse ternary operator
  bool :: a -> a -> b -> a
  bool fv tv b = B.bool fv tv $ isTruthy b

instance Truthy Bool where
  isTruthy  = id

count :: (Foldable collection, Truthy b, Additive a, P.Num a) => (Element collection -> b) -> collection -> a
count p = foldl' (\n x -> p x ? n + 1 $ n) 0
