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
import Data.Char

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

  null :: m -> Bool
  any :: (a -> Bool) -> m -> Bool
  all :: (a -> Bool) -> m -> Bool

  filter :: (a -> Bool) -> m -> m 
  find :: (a -> Bool) -> m -> Maybe a

  foldl :: (b -> a -> b) -> b -> m -> b 
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

  chunksOf :: Int -> m -> [m] 
  empty :: m

  unsnoc :: m -> Maybe (m, a)
 
  partition :: (a -> Bool) -> m -> (m, m) 
  break :: (a -> Bool) -> m -> (m, m) 
  breakOn :: Eq a => m -> m -> (m, m) 
  breakOnEnd :: Eq a => m -> m -> (m, m) 
  span :: (a -> Bool) -> m -> (m, m) 

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
  replicate :: Int -> m -> m
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
  uncons :: m -> Maybe (a, m) 
  zipWithSimple :: (a -> a -> a) -> m -> m -> m

instance Listy T.Text Char where
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

  foldl :: (b -> Char -> b) -> b -> T.Text -> b 
  foldl = T.foldl' -- note defaulting to strict

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

  findIndex :: (Char -> Bool) -> T.Text -> Maybe Int
  findIndex = T.findIndex

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

  replicate :: Int -> T.Text -> T.Text
  replicate = T.replicate

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