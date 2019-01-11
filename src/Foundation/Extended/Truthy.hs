module Foundation.Extended.Truthy (
  Truthy(..)
) where

import qualified Data.Bool  as B
import           Foundation hiding (not, (&&), (||))

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
