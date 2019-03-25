module Listy (
  Listy(..),
  countText,
  concatFoldable,
  concatMapFoldable,
  andFoldable,
  orFoldable,
  anyFoldable,
  allFoldable,
  findFoldable,
  foldrFoldable,
  nullFoldable,
  lengthFoldable,
  elemFoldable,
  foldlFoldable,
  foldlFoldableLazy,
  foldr1Foldable,
  maximumFoldable, 
  minimumFoldable,
  foldl1FoldableLazy,
  unsafeFoldr1Foldable,
  unsafeMaximumFoldable,
  unsafeMinimumFoldable,
  unsafeFoldl1FoldableLazy

) where

import qualified Data.List as L

import Data.Bool as B
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Foldable as F
import Data.Int as I
import Data.Maybe
import Data.Eq
import Data.Ord
import Data.Function

-- TODO: check reexported list functions: zip, scanl, scanr, zipWith, index
-- TODO: hide count in text an lazy -- Text -> Text -> Int -- hide sub-count

countText :: T.Text -> T.Text -> Int
countText = T.count

concatFoldable ::  Foldable t => t [a] -> [a]
concatFoldable = L.concat

concatMapFoldable :: Foldable t => (a -> [b]) -> t a -> [b]
concatMapFoldable = L.concatMap

andFoldable :: Foldable t => t Bool -> Bool
andFoldable = L.and

orFoldable :: Foldable t => t Bool -> Bool
orFoldable = L.or

anyFoldable :: Foldable t => (a -> Bool) -> t a -> Bool
anyFoldable = L.any

allFoldable :: Foldable t => (a -> Bool) -> t a -> Bool
allFoldable = L.all

findFoldable :: Foldable t => (a -> Bool) -> t a -> Maybe a 
findFoldable = L.find

--- Foldable Modifications ---

-- left alone - not relevant as not applicable to m
-- fold :: Monoid m => t m -> m
-- foldr' :: (a -> b -> b) -> b -> t a -> b
-- foldMap :: Monoid m => (a -> m) -> t a -> m
-- sum :: Num a => t a -> a
-- product :: Num a => t a -> a 

-- renamed foldable more general versions than listy
foldrFoldable :: Foldable t => (a -> b -> b) -> b -> t a -> b 
foldrFoldable = L.foldr

nullFoldable :: Foldable t => t a -> Bool
nullFoldable = L.null

lengthFoldable :: Foldable t => t a -> Int
lengthFoldable = L.length

-- TODO:: elem infix 4 -- experiment see if need this
elemFoldable :: Foldable t => Eq a => a -> t a -> Bool
elemFoldable = L.elem

-- renamed maded strict
foldlFoldable :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlFoldable = L.foldl'

-- hidden due to renaming (above)
-- foldl' :: (b -> a -> b) -> b -> t a -> b

-- old versions explicitly lazy
foldlFoldableLazy :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlFoldableLazy = L.foldl

-- safe and unsave versions of unsafe functions
-- safe

safeF3Shared :: (t -> Bool) -> ((a -> a -> a) -> t -> a) -> (a -> a -> a) -> t -> Maybe a
safeF3Shared nullFunc unsafeFunc accmFunc targ = bool 
                                                  (Just $ unsafeFunc accmFunc targ)
                                                  Nothing
                                                  $ nullFunc targ

safef3 :: Foldable t => ((a -> a -> a) -> t a -> a) -> (a -> a -> a) -> t a -> Maybe a
safef3 = safeF3Shared F.null

foldr1Foldable :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldr1Foldable = safef3 unsafeFoldr1Foldable

safeFShared :: (t -> Bool) -> (t -> a) -> t -> Maybe a
safeFShared nullFunc unsafeFunc targ = bool 
                                        (Just $ unsafeFunc targ) 
                                        Nothing
                                        (nullFunc targ)  
                                        

safef :: Foldable t => (t a -> a) -> t a -> Maybe a
safef = safeFShared F.null

maximumFoldable :: Foldable t => Ord a => t a -> Maybe a
maximumFoldable = safef unsafeMaximumFoldable

minimumFoldable :: Foldable t => Ord a => t a ->  Maybe a
minimumFoldable = safef unsafeMinimumFoldable

foldl1FoldableLazy :: Foldable t => (a -> a -> a) -> t a ->  Maybe a
foldl1FoldableLazy = safef3 unsafeFoldl1FoldableLazy

-- unsafe
unsafeFoldr1Foldable :: Foldable t => (a -> a -> a) -> t a -> a
unsafeFoldr1Foldable = L.foldr1

unsafeMaximumFoldable :: Foldable t => Ord a => t a -> a
unsafeMaximumFoldable = L.maximum

unsafeMinimumFoldable :: Foldable t => Ord a => t a -> a
unsafeMinimumFoldable = L.minimum

unsafeFoldl1FoldableLazy :: Foldable t =>  (a -> a -> a) -> t a -> a
unsafeFoldl1FoldableLazy = F.foldl1

safel :: Listy m a => (m -> b) -> m -> Maybe b
safel = safeFShared Listy.null

safeFold1l :: Listy m a => ((a -> a -> a) -> m -> a) -> (a -> a -> a) -> m -> Maybe a
safeFold1l = safeF3Shared Listy.null

class Listy m a | m -> a where
  concat :: [m] -> m 
  concatMap :: (a -> m) -> m -> m

  groupBy :: (a -> a -> Bool) -> m -> [m]
  group :: Eq a => m -> [m]

  reverse :: m -> m
  dropWhile :: (a -> Bool) -> m -> m

  -- note change to increase safety
  head :: m -> Maybe a
  head = safel unsafeHead

  last :: m -> Maybe a
  last = safel unsafeLast

  tail :: m -> Maybe m
  tail = safel unsafeTail

  init :: m -> Maybe m
  init = safel unsafeInit

  maximum :: (Ord a) => m -> Maybe a
  maximum = safel unsafeMaximum

  minimum :: (Ord a) => m -> Maybe a 
  minimum = safel unsafeMinimum 

  -- note change
  unsafeHead :: m -> a
  unsafeLast :: m -> a
  unsafeTail :: m -> m
  unsafeInit :: m -> m
  unsafeMaximum :: (Ord a) => m -> a
  unsafeMinimum :: (Ord a) => m -> a 

  -- no safe version
  unsafeIndex :: m -> Int -> a

  null :: m -> bool
  and :: (a -> Bool) -> m -> Bool
  any :: (a -> Bool) -> m -> Bool
  all :: (a -> Bool) -> m -> Bool

  filter :: (a -> Bool) -> m -> m 
  find :: (a -> Bool) -> m -> Maybe a

  foldl :: (b -> a -> b) -> b -> m -> a 
  foldlLazy :: (b -> a -> b) -> b -> m -> a 

  foldl1 :: (a -> a -> a) -> m -> Maybe a
  foldl1 = safeFold1l unsafeFoldl1

  foldl1Lazy :: (a -> a -> a) -> m -> Maybe a
  foldl1Lazy = safeFold1l unsafeFoldl1Lazy

  unsafeFoldl1 :: (a -> a -> a) -> m -> a
  unsafeFoldl1Lazy :: (a -> a -> a) -> m -> a

  foldr :: (a -> b -> b) -> b -> m -> b 

  foldr1 :: (a -> a -> a) -> m -> Maybe a
  foldr1 = safeFold1l unsafeFoldr1

  unsafeFoldr1 :: (a -> a -> a) -> m -> a

  -- note difference so m and list 
  -- use functor instance for origonal implementation 
  -- of list map
  mapSimple :: (a -> a) -> m -> m
  zipSimple :: m -> m -> [(a, a)] 

  chunksOf :: Int -> m -> [m] 
  empty :: m

  unsnoc :: m -> Maybe (m, a)
 
  partition :: (a -> Bool) -> m -> (m, m) 
  break :: (a -> Bool) -> m -> (m, m) 
  breakOn :: Eq a => m -> m -> (m, m) 
  breakOnEnd :: Eq a => m -> m -> (m, m) 
  span :: (m -> Bool) -> m -> (m, m) 

  takeEnd :: Int -> m -> m
  takeWhileEnd :: (a -> Bool) -> m -> m

  splitOn :: Eq a => m -> m -> [m]
  split :: (a -> Bool) -> m -> [m] 

  dropWhileEnd :: (a -> Bool) -> m -> m 
  findIndex :: (a -> Bool) -> m -> Maybe Int
  inits :: m -> [m]
  intercalate :: m -> [m] -> m
  intersperse :: a -> m -> m
  isInfixOf :: (Eq a) => m -> m -> Bool
  isPrefixOf :: (Eq a) => m -> m -> Bool
  isSuffixOf :: (Eq a) => m -> m -> Bool
  stripPrefix :: Eq a => m -> m -> Maybe m
  stripSuffix :: Eq a => m -> m -> Maybe m
  tails :: m -> [m]
  transpose :: [m] -> [m]
  unfoldr :: (b -> Maybe (a, b)) -> b -> m
  mapAccumL :: (b -> a -> (b, a)) -> b -> m -> (b, m) 
  mapAccumR :: (b -> a -> (b, a)) -> b -> m -> (b, m) 
  drop :: Int -> m -> m
  replicate :: Int -> a -> m
  length :: m -> Int
  scanlSimple :: (a -> a -> a) -> a -> m -> m
  scanl1 :: (a -> a -> a) -> m -> m 
  scanrSimple :: (a -> a -> a) -> a -> m -> m 
  scanr1 :: (a -> a -> a) -> m -> m 
  splitAt :: Int -> m -> (m, m)
  take :: Int -> m -> m
  takeWhile :: (a -> Bool) -> m -> m
  cons :: a -> m -> m
  snoc :: m -> a -> m
  uncons :: m -> Maybe (a, mapAccumL) 
  zipWithSimple :: (a -> a -> a) -> m -> m -> m