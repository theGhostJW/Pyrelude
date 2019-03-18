module Stringy (
  module Data.String.Encode,
  toS
) where

import Data.String.Encode 

toS :: ConvertString a b => a -> b
toS = convertString