{-# OPTIONS -fno-warn-x-partial #-}

module Listy  (
  module ExtraExport,
  Listy.last,
  Listy.init,
  Listy.maximum,
  Listy.minimum,
  Listy.head,
  Listy.tail,
  count,
  zipWithIndex,
) where

--  shims for relude to ultimately be included in a revived pyrelude

import Data.List.Extra hiding (head, tail, init, last, maximum, minimum, firstJust)
import Data.List.Extra as ExtraExport hiding (head, tail, init, last, maximum, minimum, firstJust, lines, unlines)
import Data.List.Extra qualified as L
import Ternary ((?))
import Prelude hiding (last)
import Data.Function ((&))
import Data.Foldable (toList)

-- need to hide in p(relude) too - lines / unlines

safel :: ([a] -> b) -> [a] -> Maybe b
safel unsafef l =
  l & \case
    [] -> Nothing
    _ -> Just $ unsafef l

last :: [a] -> Maybe a
last = safel L.last

head :: [a] -> Maybe a
head = safel L.head

tail :: [a] -> Maybe [a]
tail = safel L.tail

init :: [a] -> Maybe [a]
init = safel L.init

maximum :: (Ord a) => [a] -> Maybe a
maximum = safel L.maximum

minimum :: (Ord a) => [a] -> Maybe a
minimum = safel L.minimum

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count p = foldl' (\n x -> p x ? succ n $ n) 0

zipWithIndex :: (Num a, Enum a, Foldable t) => t b -> [(a, b)]
zipWithIndex = zip [0 ..] . toList