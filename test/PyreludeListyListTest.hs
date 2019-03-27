
module PyreludeListyListTest where

import Pyrelude as P hiding (Text)
import qualified Pyrelude as T (Text) 
import qualified Prelude as OP
import           Pyrelude.Test as TST
import qualified Data.Char as C

--- simple smoke tests just to make sure wiring is correct
-- concatFoldable ::  Foldable t => t [a] -> [a]
unit_concatFoldable = l "abcdef" ... concatFoldable  (l <$> ["ab","cd", "ef"])

--concatMapFoldable :: Foldable t => (a -> [b]) -> t a -> [b]
unit_concatMapFoldable =  l "ABCDE" ... concatMapFoldable (\c -> [C.toUpper c]) (l "abcde")

--anyFoldable :: Foldable t => (a -> Bool) -> t a -> Bool
unit_anyFoldable = chk $ anyFoldable (== 3) [1..5]

--allFoldable :: Foldable t => (a -> Bool) -> t a -> Bool
unit_allFoldable_empty = chk $ allFoldable (== 3) []
unit_allFoldable = chk $ allFoldable (== 3) [3, 3, 3]

-- findFoldable :: Foldable t => (a -> Bool) -> t a -> Maybe a 
unit_findFoldable = Just 3 ... findFoldable (== 3) [1..5]

-- renamed foldable more general versions than listy
-- foldrFoldable :: Foldable t => (a -> b -> b) -> b -> t a -> b 
unit_foldrFoldable = l "abc" ... foldrFoldable cons "" $ l "abc"

--nullFoldable :: Foldable t => t a -> Bool
unit_nullFoldable = chk $ nullFoldable []

--lengthFoldable :: Foldable t => t a -> Int
unit_lengthFoldable = 5 ... lengthFoldable [1, 2, 3, 4, 5]

-- elemFoldable :: Foldable t => Eq a => a -> t a -> Bool
unit_elemFoldable = chk $ elemFoldable 4 [1, 2, 3, 4, 5]

-- renamed maded strict
--foldlFoldable :: Foldable t => (b -> a -> b) -> b -> t a -> b
unit_foldlFoldable = 5 ... foldlFoldable (\a _ -> a + 1) 0  $ l "12345"

-- old versions explicitly lazy
-- foldlFoldableLazy :: Foldable t => (b -> a -> b) -> b -> t a -> b
unit_foldlFoldableLazy = 5 ... foldlFoldableLazy (\a _ -> a + 1) 0  $ l "12345"

-- foldr1Foldable :: Foldable t => (a -> a -> a) -> t a -> Maybe a
unit_foldr1Foldable = Just 'a' ... foldr1Foldable const $ l "abc"

-- maximumFoldable :: Foldable t => Ord a => t a -> Maybe a
unit_maximumFoldable = Just 6 ... maximumFoldable [1,2, 6, 3, 5]

--minimumFoldable :: Foldable t => Ord a => t a ->  Maybe a
unit_minimumFoldable = Just 1 ... minimumFoldable [1,2, 6, 3, 5]

default (Int, T.Text)

l :: T.Text -> OP.String
l = unpack

--- Listy ---
-- concat :: [T.Text] -> T.Text 
unit_concat = l "abcdef" ... concat  (l <$> ["ab","cd", "ef"])

--   concatMap :: (Char -> T.Text) -> T.Text -> T.Text
unit_concatMap = l "ABCDE" ... concatMap (\c -> [C.toUpper c]) (l "abcde")

--groupBy :: (Char -> Char -> Bool) -> T.Text -> [T.Text]
{-# ANN unit_groupBy ("HLint: ignore Use group" :: OP.String) #-}
unit_groupBy = l <$> ["aaa", "bbb", "cc"] ... groupBy (==) $ l "aaabbbcc"

-- group :: T.Text -> [T.Text]
unit_group = l <$> ["aaa", "bbb", "cc"] ... group $ l "aaabbbcc"

--   reverse :: T.Text -> T.Text
unit_reverse = l "cba" ... reverse $ l "abc"

--   dropWhile :: (Char -> Bool) -> T.Text -> T.Text
unit_dropWhile = l "de" ... dropWhile (<= 'c') $ l"abcde"

--   -- note change to increase safety
--   head :: T.Text -> Maybe Char
unit_head = Just 'a' ... head $ l "abc"

-- last :: T.Text -> Maybe Char
unit_last =  Just 'c' ... last $ l "abc"

--   tail :: T.Text -> Maybe T.Text
unit_tail = Just (l "bc") ... tail $ l"abc"

--   init :: T.Text -> Maybe T.Text
unit_init = Just "ab" ... init "abc"

--   maximum :: (Ord Char) => T.Text -> Maybe Char
unit_maximum = Just 'c' ... maximum $ l "abc"

--   minimum :: (Ord Char) => T.Text -> Maybe Char 
unit_minimum = Just 'a' ... minimum $ l "abc"

--   -- no safe version
--   unsafeIndex :: T.Text -> Int -> Char
unit_unsafeIndex = 'c' ... unsafeIndex (l "abc") 2

--   null :: T.Text -> Bool
unit_null = chk $ null []

-- any :: (Char -> Bool) -> T.Text -> Bool
unit_any = chk $ any (== 'a') $ l "xyzab"

--   all :: (Char -> Bool) -> T.Text -> Bool
unit_all = chkFalse $ all (== 'a') $ l "aaaab"

--   filter :: (Char -> Bool) -> T.Text -> T.Text 
unit_filter = "aaa" ... P.filter (== 'a') $ l  "afgrdavgwatyu" 

--   find :: (Char -> Bool) -> T.Text -> Maybe Char
unit_find = Just 'a' ... P.find (== 'a') $ l  "afgrdavgwatyu" 

--   foldl :: (b -> Char -> b) -> b -> T.Text -> b 
unit_foldl = 5 ... foldl (\a _ -> a + 1) 0  $ l "12345"

-- foldlLazy :: (b -> Char -> b) -> b -> T.Text -> b 
unit_foldlLazy = 5 ... foldlLazy (\a _ -> a + 1) 0 $ l "12345"

--   foldl1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldl1 = Just '5' ... foldl1 (\_ a -> a) $ l "12345"
unit_foldl1_empty = Nothing ... foldl1 (\_ a -> a)  []

--   foldl1Lazy :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldl1Lazy = Just '5' ... foldl1Lazy (\_ a -> a) $ l "12345"
unit_foldl1Lazy_empty = Nothing ... foldl1Lazy (\_ a -> a) []

-- foldr :: (Char -> b -> b) -> b -> T.Text -> b 
unit_foldr = l "abc" ... foldr cons "" $ l "abc"

--   foldr1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldr1 = Just 'a' ... foldr1 const $ l "abc"
unit_foldr1_empty = Nothing ... foldr1 const []

--   -- note difference so T.Text and list 
--   -- use functor instance for origonal implementation 
--   -- of list map
--   mapSimple :: (Char -> Char) -> T.Text -> T.Text
unit_mapSimple = l "ABC" ... mapSimple C.toUpper $ l "aBc"

--   zipSimple :: T.Text -> T.Text -> [(Char, Char)] 
unit_zipSimple = [('a', 'x'), ('b', 'y'), ('c', 'z')] ... zipSimple (l "abc") (l "xyz")

--   chunksOf :: Int -> T.Text -> [T.Text] 
unit_chunksOf = [l "ab", l "cd", l "e"] ... chunksOf 2 $ l "abcde"

--   empty :: T.Text
unit_empty = [] ... empty

--   unsnoc :: T.Text -> Maybe (T.Text, Char)
unit_unsnoc = Just (l "abc", 'd') ... unsnoc $ l "abcd"
 
--   partition :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_unit_partition = (l "aaa", l "bbyb") ... partition ('a' ==) $ l "ababyab"

--   break :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_break = (l "ab", l "cde") ... break (== 'c') $ l "abcde"

-- Eq a => [a] -> [a] -> ([a], [a]) 
unit_breakOn = (["ab"], ["cd", "e", "fg"]) ... breakOn ["cd"] ["ab", "cd", "e", "fg"]

--   breakOnEnd :: T.Text -> T.Text -> (T.Text, T.Text) 
unit_breakOnEnd = (["ab", "cd", "e", "cd"], ["fg"]) ... breakOnEnd ["cd"] ["ab", "cd", "e", "cd", "fg"]

--   span :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_span = (l "abcd", l "efghij") ... span (< 'e') $ l "abcdefghij"

--   takeEnd :: Int -> T.Text -> T.Text
unit_takeEnd = l "fgh" ... takeEnd 3 $ l "bcdefgh"

--   takeWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
unit_takeWhileEnd = l "dcba" ... takeWhileEnd (< 'e') $ l "gfedcba"

--   splitOn :: T.Text -> T.Text -> [T.Text]
unit_splitOn = [l "abcd", l "ghi"] ... splitOn (l "ef") $ l "abcdefghi"

--   split :: (Char -> Bool) -> T.Text -> [T.Text] 
unit_split = [l "abcd", l "fg" , l "hi"] ... split (== 'e') $ l "abcdefgehi"

--   dropWhileEnd :: (Char -> Bool) -> T.Text -> T.Text 
unit_dropWhileEnd = l "abcdefg" ... dropWhileEnd (> 'g') $ l "abcdefghi"

--   inits :: T.Text -> [T.Text]
unit_inits = l <$> ["","a","ab","abc","abcd","abcde","abcdef","abcdefg"] ... inits $ l "abcdefg"

--   intercalate :: T.Text -> [T.Text] -> T.Text
unit_intercalate = l "wasabcsawabcMe" ... intercalate (l "abc") $ l <$> ["was", "saw", "Me"]

--   intersperse :: Char -> T.Text -> T.Text
unit_intersperse = l "axbxc" ... intersperse 'x' $ l "abc"

--   isInfixOf :: T.Text -> T.Text -> Bool
unit_isInfixOf = chk $ isInfixOf (l "ef") $ l "abcdefgh" 

-- isPrefixOf :: T.Text -> T.Text -> Bool
unit_isPrefixOf = chk $ isPrefixOf (l "ab") $ l "abcdefgh"

--   isSuffixOf :: T.Text -> T.Text -> Bool
unit_isSuffixOf = chk $ isSuffixOf (l "gh") $ l "abcdefgh"

--   stripPrefix :: T.Text -> T.Text -> Maybe T.Text
unit_stripPrefix = Just "cdefgh" ... stripPrefix (l "ab") (l "abcdefgh")

--   stripSuffix :: T.Text -> T.Text -> Maybe T.Text
unit_stripSuffix = Just (l "abcdef") ... stripSuffix "gh" $ l "abcdefgh"

--   tails :: T.Text -> [T.Text]
unit_tails = l <$> ["abcdefgh","bcdefgh","cdefgh","defgh","efgh","fgh","gh","h",""] ... tails $ l "abcdefgh"

--   transpose :: [T.Text] -> [T.Text]
unit_transpose = l <$> ["go","rr","ea","en","ng","e"] ... transpose [l "green", l "orange"]

--   unfoldr :: (b -> Maybe (Char, b)) -> b -> T.Text
unit_unfoldr = l "zyxwvutsrqponmlkjihgfedcba" ... unfoldr (\b -> if b < 97 then Nothing else Just (chr b, b-1)) 122

--   mapAccumL :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
unit_mapAccumL = (9, l "bcdefghij") ... mapAccumL (\b c -> (b + 1, chr $ b + 98)) 0 $ l "abcdefghi"

--   mapAccumR :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
unit_mapAccumR =  (9, l "iiiiiiiii") ... mapAccumR  (\b c -> (b + 1, chr . (+) b $ ord c)) 0 $ l"abcdefghi"

--   drop :: Int -> T.Text -> T.Text
unit_drop = l "defghi" ... drop 3 $ l "abcdefghi"

--   length :: T.Text -> Int
unit_length = 9 ... length $ l "abcdefghi"

--   scanlSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
unit_scanlSimple = l "aabcdef" ... scanlSimple (\a b -> if b > a then b else a) 'a' $ l "abcdef"

--   scanl1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
unit_scanl1_empty = "" ... scanl1 (\a b -> if b > a then b else a)  []
unit_scanl1 = l "aaaaaa" ... scanl1 (\a b -> if b < a then b else a) $ l "abcdef"

--   scanrSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text 
unit_scanrSimple = l "ffffffa" ... scanrSimple (\a b -> if b > a then b else a) 'a' "abcdef"

--   scanr1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
unit_scanr1_empty = [] ... scanr1 (\a b -> if b > a then b else a) []
unit_scanr1 = l "ffffff" ... scanr1 (\a b -> if b > a then b else a) $ l "abcdef" 

--   splitAt :: Int -> T.Text -> (T.Text, T.Text)
unit_splitAt_outOfBounds = (l "abcde", l "") ... splitAt 10 $ l "abcde"
unit_splitAt = (l "ab", l "cde") ... splitAt 2 $ l"abcde"

--   take :: Int -> T.Text -> T.Text
unit_take = l "abc" ... take 3 $ l "abcderfdfdfds"

--   takeWhile :: (Char -> Bool) -> T.Text -> T.Text
unit_takeWhile = l "abcd" ... takeWhile (< 'e') $ l "abcdefgh"

--   cons :: Char -> T.Text -> T.Text
unit_cons = l "abcde" ... cons 'a' $ l "bcde"

--   snoc :: T.Text -> Char -> T.Text
unit_snoc = l "abcde" ... snoc (l "abcd") 'e'

--   uncons :: T.Text -> Maybe (Char, T.Text) 
unit_uncons = Just ('a', l "bcde") ... uncons $ l "abcde"

--   zipWithSimple :: (Char -> Char -> Char) -> T.Text -> T.Text -> T.Text
unit_zipWithSimple = l "cccde" ... zipWith (\a b -> if b > a then b else a)  (l "abcde") $ l "cccccccccccc"