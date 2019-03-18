module Ternary (
  (?)
) where

import BasePrelude

infixl 1 ?
(?) :: Bool -> a -> a -> a
(?) b a1 a2 = if b then a1 else a2