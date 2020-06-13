{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Listy (
  Listy(..),
  countText,
  countTextLazy,
  concatFoldable,
  concatMapFoldable,
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
  unsafeFoldl1FoldableLazy,
  replicateText,
  replicateTextLazy,
  findIndexText
) where

import qualified Data.List as L

import Data.Bool as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Foldable as F
import Data.Int as I
import Data.Maybe
import Data.Eq
import Data.Ord
import Data.Function
import Data.Char
import Data.List.Extra as E
import GHC.Real

-- TODO: hide count in text an lazy -- Text -> Text -> Int -- hide sub-count

countText :: T.Text -> T.Text -> Int
countText = T.count

countTextLazy :: LT.Text -> LT.Text -> Int64
countTextLazy = LT.count

concatFoldable ::  Foldable t => t [a] -> [a]
concatFoldable = L.concat

concatMapFoldable :: Foldable t => (a -> [b]) -> t a -> [b]
concatMapFoldable = L.concatMap

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


infix 4 `elemFoldable`
elemFoldable :: Foldable t => Eq a => a -> t a -> Bool
elemFoldable = L.elem

-- renamed maded strict
foldlFoldable :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlFoldable = L.foldl'

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

safel :: Listy m a i => (m -> b) -> m -> Maybe b
safel = safeFShared Listy.null

safeFold1l :: Listy m a i => ((a -> a -> a) -> m -> a) -> (a -> a -> a) -> m -> Maybe a
safeFold1l = safeF3Shared Listy.null

replicateText :: Int -> T.Text -> T.Text
replicateText = T.replicate

replicateTextLazy :: Int64 -> LT.Text -> LT.Text
replicateTextLazy = LT.replicate 

findIndexText :: (Char -> Bool) -> T.Text -> Maybe Int
findIndexText = T.findIndex

class Integral i => Listy m a i | m -> a i where
  concat :: [m] -> m 
  concatMap :: (a -> m) -> m -> m

  --  https://stackoverflow.com/questions/14922070/haskell-use-data-text-replace-to-replace-only-the-first-occurrence-of-a-text-va
  replaceFirst :: Eq a => m -- ^ needle
                       -> m -- ^ replacement
                       -> m -- ^ haystack
                       -> m
  replaceFirst needle replacement haystack
    | Listy.null back = haystack 
    | otherwise = Listy.concat [front, replacement, Listy.drop (Listy.length needle) back] 
      where
        (front, back) = Listy.breakOn needle haystack

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
  unsafeIndex :: m -> i -> a

  null :: m -> Bool
  any :: (a -> Bool) -> m -> Bool
  all :: (a -> Bool) -> m -> Bool

  filter :: (a -> Bool) -> m -> m 
  find :: (a -> Bool) -> m -> Maybe a

  foldl' :: (b -> a -> b) -> b -> m -> b 

  foldlLazy :: (b -> a -> b) -> b -> m -> b 

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

  chunksOf :: i -> m -> [m] 
  empty :: m

  unsnoc :: m -> Maybe (m, a)
 
  partition :: (a -> Bool) -> m -> (m, m) 
  break :: (a -> Bool) -> m -> (m, m) 
  breakOn :: Eq a => m -> m -> (m, m) 
  breakOnEnd :: Eq a => m -> m -> (m, m) 
  span :: (a -> Bool) -> m -> (m, m) 

  takeEnd :: i -> m -> m
  takeWhileEnd :: (a -> Bool) -> m -> m

  splitOn :: Eq a => m -> m -> [m]
  split :: (a -> Bool) -> m -> [m] 

  dropWhileEnd :: (a -> Bool) -> m -> m 
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
  drop :: i -> m -> m
  length :: m -> i
  scanlSimple :: (a -> a -> a) -> a -> m -> m
  scanl1 :: (a -> a -> a) -> m -> m 
  scanrSimple :: (a -> a -> a) -> a -> m -> m 
  scanr1 :: (a -> a -> a) -> m -> m 
  splitAt :: i -> m -> (m, m)
  take :: i -> m -> m
  takeWhile :: (a -> Bool) -> m -> m
  cons :: a -> m -> m
  snoc :: m -> a -> m
  uncons :: m -> Maybe (a, m) 
  zipWithSimple :: (a -> a -> a) -> m -> m -> m

instance Listy T.Text Char Int where
  concat :: [T.Text] -> T.Text 
  concat = T.concat

  concatMap :: (Char -> T.Text) -> T.Text -> T.Text
  concatMap = T.concatMap

  groupBy :: (Char -> Char -> Bool) -> T.Text -> [T.Text]
  groupBy = T.groupBy

  group :: T.Text -> [T.Text]
  group = T.group

  reverse :: T.Text -> T.Text
  reverse = T.reverse

  dropWhile :: (Char -> Bool) -> T.Text -> T.Text
  dropWhile = T.dropWhile

  -- note change to increase safety
  head :: T.Text -> Maybe Char
  head = safel unsafeHead

  last :: T.Text -> Maybe Char
  last = safel unsafeLast

  tail :: T.Text -> Maybe T.Text
  tail = safel unsafeTail

  init :: T.Text -> Maybe T.Text
  init = safel unsafeInit

  maximum :: (Ord Char) => T.Text -> Maybe Char
  maximum = safel unsafeMaximum

  minimum :: (Ord Char) => T.Text -> Maybe Char 
  minimum = safel unsafeMinimum 

  -- note change
  unsafeHead :: T.Text -> Char
  unsafeHead = T.head

  unsafeLast :: T.Text -> Char
  unsafeLast = T.last

  unsafeTail :: T.Text -> T.Text
  unsafeTail = T.tail

  unsafeInit :: T.Text -> T.Text
  unsafeInit = T.init

  unsafeMaximum :: T.Text -> Char
  unsafeMaximum = T.maximum

  unsafeMinimum :: T.Text -> Char 
  unsafeMinimum = T.minimum 

  -- no safe version
  unsafeIndex :: T.Text -> Int -> Char
  unsafeIndex = T.index

  null :: T.Text -> Bool
  null = T.null

  any :: (Char -> Bool) -> T.Text -> Bool
  any = T.any

  all :: (Char -> Bool) -> T.Text -> Bool
  all = T.all

  filter :: (Char -> Bool) -> T.Text -> T.Text 
  filter = T.filter 

  find :: (Char -> Bool) -> T.Text -> Maybe Char
  find = T.find

  foldl' :: (b -> Char -> b) -> b -> T.Text -> b 
  foldl' = T.foldl'

  foldlLazy :: (b -> Char -> b) -> b -> T.Text -> b 
  foldlLazy = T.foldl

  foldl1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
  foldl1 = safeFold1l unsafeFoldl1

  foldl1Lazy :: (Char -> Char -> Char) -> T.Text -> Maybe Char
  foldl1Lazy = safeFold1l unsafeFoldl1Lazy

  unsafeFoldl1 :: (Char -> Char -> Char) -> T.Text -> Char
  unsafeFoldl1 = T.foldl1'

  unsafeFoldl1Lazy :: (Char -> Char -> Char) -> T.Text -> Char
  unsafeFoldl1Lazy = T.foldl1

  foldr :: (Char -> b -> b) -> b -> T.Text -> b 
  foldr = T.foldr

  foldr1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
  foldr1 = safeFold1l unsafeFoldr1

  unsafeFoldr1 :: (Char -> Char -> Char) -> T.Text -> Char
  unsafeFoldr1 = T.foldr1

  -- note difference so T.Text and list 
  -- use functor instance for origonal implementation 
  -- of list map
  mapSimple :: (Char -> Char) -> T.Text -> T.Text
  mapSimple = T.map

  zipSimple :: T.Text -> T.Text -> [(Char, Char)] 
  zipSimple = T.zip

  chunksOf :: Int -> T.Text -> [T.Text] 
  chunksOf = T.chunksOf

  empty :: T.Text
  empty = T.empty

  unsnoc :: T.Text -> Maybe (T.Text, Char)
  unsnoc = T.unsnoc
 
  partition :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
  partition = T.partition

  break :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
  break = T.break

  breakOn :: T.Text -> T.Text -> (T.Text, T.Text) 
  breakOn = T.breakOn

  breakOnEnd :: T.Text -> T.Text -> (T.Text, T.Text) 
  breakOnEnd = T.breakOnEnd

  span :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
  span = T.span

  takeEnd :: Int -> T.Text -> T.Text
  takeEnd = T.takeEnd

  takeWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
  takeWhileEnd = T.takeWhileEnd

  splitOn :: T.Text -> T.Text -> [T.Text]
  splitOn = T.splitOn

  split :: (Char -> Bool) -> T.Text -> [T.Text] 
  split = T.split

  dropWhileEnd :: (Char -> Bool) -> T.Text -> T.Text 
  dropWhileEnd = T.dropWhileEnd

  inits :: T.Text -> [T.Text]
  inits = T.inits

  intercalate :: T.Text -> [T.Text] -> T.Text
  intercalate = T.intercalate

  intersperse :: Char -> T.Text -> T.Text
  intersperse = T.intersperse

  isInfixOf :: T.Text -> T.Text -> Bool
  isInfixOf = T.isInfixOf

  isPrefixOf :: T.Text -> T.Text -> Bool
  isPrefixOf = T.isPrefixOf

  isSuffixOf :: T.Text -> T.Text -> Bool
  isSuffixOf = T.isSuffixOf

  stripPrefix :: T.Text -> T.Text -> Maybe T.Text
  stripPrefix = T.stripPrefix

  stripSuffix :: T.Text -> T.Text -> Maybe T.Text
  stripSuffix = T.stripSuffix

  tails :: T.Text -> [T.Text]
  tails = T.tails

  transpose :: [T.Text] -> [T.Text]
  transpose = T.transpose

  unfoldr :: (b -> Maybe (Char, b)) -> b -> T.Text
  unfoldr = T.unfoldr

  mapAccumL :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
  mapAccumL = T.mapAccumL

  mapAccumR :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
  mapAccumR = T.mapAccumR

  drop :: Int -> T.Text -> T.Text
  drop = T.drop

  length :: T.Text -> Int
  length = T.length

  scanlSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
  scanlSimple = T.scanl

  scanl1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
  scanl1 = T.scanl1

  scanrSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text 
  scanrSimple = T.scanr

  scanr1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
  scanr1 = T.scanr1

  splitAt :: Int -> T.Text -> (T.Text, T.Text)
  splitAt = T.splitAt

  take :: Int -> T.Text -> T.Text
  take = T.take 

  takeWhile :: (Char -> Bool) -> T.Text -> T.Text
  takeWhile = T.takeWhile

  cons :: Char -> T.Text -> T.Text
  cons = T.cons

  snoc :: T.Text -> Char -> T.Text
  snoc = T.snoc

  uncons :: T.Text -> Maybe (Char, T.Text) 
  uncons = T.uncons

  zipWithSimple :: (Char -> Char -> Char) -> T.Text -> T.Text -> T.Text
  zipWithSimple = T.zipWith

instance Listy LT.Text Char Int64 where
  concat :: [LT.Text] -> LT.Text 
  concat = LT.concat

  concatMap :: (Char -> LT.Text) -> LT.Text -> LT.Text
  concatMap = LT.concatMap

  groupBy :: (Char -> Char -> Bool) -> LT.Text -> [LT.Text]
  groupBy = LT.groupBy

  group :: LT.Text -> [LT.Text]
  group = LT.group

  reverse :: LT.Text -> LT.Text
  reverse = LT.reverse

  dropWhile :: (Char -> Bool) -> LT.Text -> LT.Text
  dropWhile = LT.dropWhile

  -- note change to increase safety
  head :: LT.Text -> Maybe Char
  head = safel unsafeHead

  last :: LT.Text -> Maybe Char
  last = safel unsafeLast

  tail :: LT.Text -> Maybe LT.Text
  tail = safel unsafeTail

  init :: LT.Text -> Maybe LT.Text
  init = safel unsafeInit

  maximum :: (Ord Char) => LT.Text -> Maybe Char
  maximum = safel unsafeMaximum

  minimum :: (Ord Char) => LT.Text -> Maybe Char 
  minimum = safel unsafeMinimum 

  -- note change
  unsafeHead :: LT.Text -> Char
  unsafeHead = LT.head

  unsafeLast :: LT.Text -> Char
  unsafeLast = LT.last

  unsafeTail :: LT.Text -> LT.Text
  unsafeTail = LT.tail

  unsafeInit :: LT.Text -> LT.Text
  unsafeInit = LT.init

  unsafeMaximum :: LT.Text -> Char
  unsafeMaximum = LT.maximum

  unsafeMinimum :: LT.Text -> Char 
  unsafeMinimum = LT.minimum 

  -- no safe version
  unsafeIndex :: LT.Text -> Int64 -> Char
  unsafeIndex = LT.index

  null :: LT.Text -> Bool
  null = LT.null

  any :: (Char -> Bool) -> LT.Text -> Bool
  any = LT.any

  all :: (Char -> Bool) -> LT.Text -> Bool
  all = LT.all

  filter :: (Char -> Bool) -> LT.Text -> LT.Text 
  filter = LT.filter 

  find :: (Char -> Bool) -> LT.Text -> Maybe Char
  find = LT.find

  foldl' :: (b -> Char -> b) -> b -> LT.Text -> b 
  foldl' = LT.foldl'

  foldlLazy :: (b -> Char -> b) -> b -> LT.Text -> b 
  foldlLazy = LT.foldl

  foldl1 :: (Char -> Char -> Char) -> LT.Text -> Maybe Char
  foldl1 = safeFold1l unsafeFoldl1

  foldl1Lazy :: (Char -> Char -> Char) -> LT.Text -> Maybe Char
  foldl1Lazy = safeFold1l unsafeFoldl1Lazy

  unsafeFoldl1 :: (Char -> Char -> Char) -> LT.Text -> Char
  unsafeFoldl1 = LT.foldl1'

  unsafeFoldl1Lazy :: (Char -> Char -> Char) -> LT.Text -> Char
  unsafeFoldl1Lazy = LT.foldl1

  foldr :: (Char -> b -> b) -> b -> LT.Text -> b 
  foldr = LT.foldr

  foldr1 :: (Char -> Char -> Char) -> LT.Text -> Maybe Char
  foldr1 = safeFold1l unsafeFoldr1

  unsafeFoldr1 :: (Char -> Char -> Char) -> LT.Text -> Char
  unsafeFoldr1 = LT.foldr1

  -- note difference so LT.Text and list 
  -- use functor instance for origonal implementation 
  -- of list map
  mapSimple :: (Char -> Char) -> LT.Text -> LT.Text
  mapSimple = LT.map

  zipSimple :: LT.Text -> LT.Text -> [(Char, Char)] 
  zipSimple = LT.zip

  chunksOf :: Int64 -> LT.Text -> [LT.Text] 
  chunksOf = LT.chunksOf

  empty :: LT.Text
  empty = LT.empty

  unsnoc :: LT.Text -> Maybe (LT.Text, Char)
  unsnoc = LT.unsnoc
 
  partition :: (Char -> Bool) -> LT.Text -> (LT.Text, LT.Text) 
  partition = LT.partition

  break :: (Char -> Bool) -> LT.Text -> (LT.Text, LT.Text) 
  break = LT.break

  breakOn :: LT.Text -> LT.Text -> (LT.Text, LT.Text) 
  breakOn = LT.breakOn

  breakOnEnd :: LT.Text -> LT.Text -> (LT.Text, LT.Text) 
  breakOnEnd = LT.breakOnEnd

  span :: (Char -> Bool) -> LT.Text -> (LT.Text, LT.Text) 
  span = LT.span

  takeEnd :: Int64 -> LT.Text -> LT.Text
  takeEnd = LT.takeEnd

  takeWhileEnd :: (Char -> Bool) -> LT.Text -> LT.Text
  takeWhileEnd = LT.takeWhileEnd

  splitOn :: LT.Text -> LT.Text -> [LT.Text]
  splitOn = LT.splitOn

  split :: (Char -> Bool) -> LT.Text -> [LT.Text] 
  split = LT.split

  dropWhileEnd :: (Char -> Bool) -> LT.Text -> LT.Text 
  dropWhileEnd = LT.dropWhileEnd

  inits :: LT.Text -> [LT.Text]
  inits = LT.inits

  intercalate :: LT.Text -> [LT.Text] -> LT.Text
  intercalate = LT.intercalate

  intersperse :: Char -> LT.Text -> LT.Text
  intersperse = LT.intersperse

  isInfixOf :: LT.Text -> LT.Text -> Bool
  isInfixOf = LT.isInfixOf

  isPrefixOf :: LT.Text -> LT.Text -> Bool
  isPrefixOf = LT.isPrefixOf

  isSuffixOf :: LT.Text -> LT.Text -> Bool
  isSuffixOf = LT.isSuffixOf

  stripPrefix :: LT.Text -> LT.Text -> Maybe LT.Text
  stripPrefix = LT.stripPrefix

  stripSuffix :: LT.Text -> LT.Text -> Maybe LT.Text
  stripSuffix = LT.stripSuffix

  tails :: LT.Text -> [LT.Text]
  tails = LT.tails

  transpose :: [LT.Text] -> [LT.Text]
  transpose = LT.transpose

  unfoldr :: (b -> Maybe (Char, b)) -> b -> LT.Text
  unfoldr = LT.unfoldr

  mapAccumL :: (b -> Char -> (b, Char)) -> b -> LT.Text -> (b, LT.Text) 
  mapAccumL = LT.mapAccumL

  mapAccumR :: (b -> Char -> (b, Char)) -> b -> LT.Text -> (b, LT.Text) 
  mapAccumR = LT.mapAccumR

  drop :: Int64 -> LT.Text -> LT.Text
  drop = LT.drop

  length :: LT.Text -> Int64
  length = LT.length

  scanlSimple :: (Char -> Char -> Char) -> Char -> LT.Text -> LT.Text
  scanlSimple = LT.scanl

  scanl1 :: (Char -> Char -> Char) -> LT.Text -> LT.Text 
  scanl1 = LT.scanl1

  scanrSimple :: (Char -> Char -> Char) -> Char -> LT.Text -> LT.Text 
  scanrSimple = LT.scanr

  scanr1 :: (Char -> Char -> Char) -> LT.Text -> LT.Text 
  scanr1 = LT.scanr1

  splitAt :: Int64 -> LT.Text -> (LT.Text, LT.Text)
  splitAt = LT.splitAt

  take :: Int64 -> LT.Text -> LT.Text
  take = LT.take 

  takeWhile :: (Char -> Bool) -> LT.Text -> LT.Text
  takeWhile = LT.takeWhile

  cons :: Char -> LT.Text -> LT.Text
  cons = LT.cons

  snoc :: LT.Text -> Char -> LT.Text
  snoc = LT.snoc

  uncons :: LT.Text -> Maybe (Char, LT.Text) 
  uncons = LT.uncons

  zipWithSimple :: (Char -> Char -> Char) -> LT.Text -> LT.Text -> LT.Text
  zipWithSimple = LT.zipWith

instance Listy [a] a Int where
  concat :: [[a]] -> [a] 
  concat = L.concat

  concatMap :: (a -> [a]) -> [a] -> [a]
  concatMap = L.concatMap

  groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
  groupBy = L.groupBy

  group :: Eq a => [a] -> [[a]]
  group = L.group

  reverse :: [a] -> [a]
  reverse = L.reverse

  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile = L.dropWhile

  -- note change to increase safety
  head :: [a] -> Maybe a
  head = safel unsafeHead

  last :: [a] -> Maybe a
  last = safel unsafeLast

  tail :: [a] -> Maybe [a]
  tail = safel unsafeTail

  init :: [a] -> Maybe [a]
  init = safel unsafeInit

  maximum :: (Ord a) => [a] -> Maybe a
  maximum = safel unsafeMaximum

  minimum :: (Ord a) => [a] -> Maybe a 
  minimum = safel unsafeMinimum 

  -- note change
  unsafeHead :: [a] -> a
  unsafeHead = L.head

  unsafeLast :: [a] -> a
  unsafeLast = L.last

  unsafeTail :: [a] -> [a]
  unsafeTail = L.tail

  unsafeInit :: [a] -> [a]
  unsafeInit = L.init

  unsafeMaximum :: (Ord a) => [a] -> a
  unsafeMaximum = L.maximum

  unsafeMinimum :: (Ord a) => [a] -> a 
  unsafeMinimum = L.minimum 

  -- no safe version
  unsafeIndex :: [a] -> Int -> a
  unsafeIndex = (!!)

  null :: [a] -> Bool
  null = L.null

  any :: (a -> Bool) -> [a] -> Bool
  any = L.any

  all :: (a -> Bool) -> [a] -> Bool
  all = L.all

  filter :: (a -> Bool) -> [a] -> [a] 
  filter = L.filter 

  find :: (a -> Bool) -> [a] -> Maybe a
  find = L.find

  foldl' :: (b -> a -> b) -> b -> [a] -> b 
  foldl' = L.foldl' -- note defaulting to strict'

  foldlLazy :: (b -> a -> b) -> b -> [a] -> b 
  foldlLazy = L.foldl

  foldl1 :: (a -> a -> a) -> [a] -> Maybe a
  foldl1 = safeFold1l unsafeFoldl1

  foldl1Lazy :: (a -> a -> a) -> [a] -> Maybe a
  foldl1Lazy = safeFold1l unsafeFoldl1Lazy

  unsafeFoldl1 :: (a -> a -> a) -> [a] -> a
  unsafeFoldl1 = L.foldl1'

  unsafeFoldl1Lazy :: (a -> a -> a) -> [a] -> a
  unsafeFoldl1Lazy = L.foldl1

  foldr :: (a -> b -> b) -> b -> [a] -> b 
  foldr = L.foldr

  foldr1 :: (a -> a -> a) -> [a] -> Maybe a
  foldr1 = safeFold1l unsafeFoldr1

  unsafeFoldr1 :: (a -> a -> a) -> [a] -> a
  unsafeFoldr1 = L.foldr1

  -- note difference so [a] and list 
  -- use functor instance for origonal implementation 
  -- of list map
  mapSimple :: (a -> a) -> [a] -> [a]
  mapSimple = L.map

  zipSimple :: [a] -> [a] -> [(a, a)] 
  zipSimple = L.zip

  chunksOf :: Int -> [a] -> [[a]] 
  chunksOf = E.chunksOf

  empty :: [a]
  empty = []

  unsnoc :: [a] -> Maybe ([a], a)
  unsnoc = E.unsnoc
 
  partition :: (a -> Bool) -> [a] -> ([a], [a]) 
  partition = L.partition

  break :: (a -> Bool) -> [a] -> ([a], [a]) 
  break = L.break

  breakOn :: Eq a => [a] -> [a] -> ([a], [a]) 
  breakOn = E.breakOn

  breakOnEnd :: Eq a => [a] -> [a] -> ([a], [a]) 
  breakOnEnd = E.breakOnEnd

  span :: (a -> Bool) -> [a] -> ([a], [a]) 
  span = E.span

  takeEnd :: Int -> [a] -> [a]
  takeEnd = E.takeEnd

  takeWhileEnd :: (a -> Bool) -> [a] -> [a]
  takeWhileEnd = E.takeWhileEnd

  splitOn :: Eq a => [a] -> [a] -> [[a]]
  splitOn = E.splitOn

  split :: (a -> Bool) -> [a] -> [[a]] 
  split = E.split

  dropWhileEnd :: (a -> Bool) -> [a] -> [a] 
  dropWhileEnd = L.dropWhileEnd

  inits :: [a] -> [[a]]
  inits = L.inits

  intercalate :: [a] -> [[a]] -> [a]
  intercalate = L.intercalate

  intersperse :: a -> [a] -> [a]
  intersperse = L.intersperse

  isInfixOf :: Eq a => [a] -> [a] -> Bool
  isInfixOf = L.isInfixOf

  isPrefixOf :: Eq a => [a] -> [a] -> Bool
  isPrefixOf = L.isPrefixOf

  isSuffixOf :: Eq a => [a] -> [a] -> Bool
  isSuffixOf = L.isSuffixOf

  stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
  stripPrefix = L.stripPrefix

  stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
  stripSuffix = E.stripSuffix

  tails :: [a] -> [[a]]
  tails = L.tails

  transpose :: [[a]] -> [[a]]
  transpose = L.transpose

  unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  unfoldr = L.unfoldr

  mapAccumL :: (b -> a -> (b, a)) -> b -> [a] -> (b, [a]) 
  mapAccumL = L.mapAccumL

  mapAccumR :: (b -> a -> (b, a)) -> b -> [a] -> (b, [a]) 
  mapAccumR = L.mapAccumR

  drop :: Int -> [a] -> [a]
  drop = L.drop

  length :: [a] -> Int
  length = L.length

  scanlSimple :: (a -> a -> a) -> a -> [a] -> [a]
  scanlSimple = L.scanl

  scanl1 :: (a -> a -> a) -> [a] -> [a] 
  scanl1 = L.scanl1

  scanrSimple :: (a -> a -> a) -> a -> [a] -> [a] 
  scanrSimple = L.scanr

  scanr1 :: (a -> a -> a) -> [a] -> [a] 
  scanr1 = L.scanr1

  splitAt :: Int -> [a] -> ([a], [a])
  splitAt = L.splitAt

  take :: Int -> [a] -> [a]
  take = L.take 

  takeWhile :: (a -> Bool) -> [a] -> [a]
  takeWhile = L.takeWhile

  cons :: a -> [a] -> [a]
  cons = E.cons

  snoc :: [a] -> a -> [a]
  snoc = E.snoc

  uncons :: [a] -> Maybe (a, [a]) 
  uncons = L.uncons

  zipWithSimple :: (a -> a -> a) -> [a] -> [a] -> [a]
  zipWithSimple = L.zipWith