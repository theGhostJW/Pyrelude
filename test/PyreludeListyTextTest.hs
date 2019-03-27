
module PyreludeListyTextTest where

import Pyrelude as P hiding (Text)
import qualified Pyrelude as T (Text) 
import qualified Prelude as OP
import           Pyrelude.Test as TST hiding (singleton)
import qualified ListyTestShared as LTS
import qualified Data.Char as C

--- simple smoke tests just to make sure wiring is correct

default (Int, T.Text)

--- Special Text Functions --
unit_countText = 3 ... countText "ab" "abcabab"
unit_replicateText = "ababab" ... replicateText 3 "ab"
unit_findIndexText = Just 5 ... findIndexText ('5' ==) "0123456"

--- Listy ---
-- concat :: [T.Text] -> T.Text 
unit_concat = "abcdef" ... concat ["ab","cd", "ef"] 

--   concatMap :: (Char -> T.Text) -> T.Text -> T.Text
unit_concatMap = "ABCDE" ... concatMap (toUpper . singleton) "abcde" 

--groupBy :: (Char -> Char -> Bool) -> T.Text -> [T.Text]
{-# ANN unit_groupBy ("HLint: ignore Use group" :: OP.String) #-}
unit_groupBy = ["aaa", "bbb", "cc"] ... groupBy (==) ("aaabbbcc" :: T.Text)

-- group :: T.Text -> [T.Text]
unit_group = ["aaa", "bbb", "cc"] ... group ("aaabbbcc" :: T.Text)

--   reverse :: T.Text -> T.Text
unit_reverse = "cba" ... reverse "abc"

--   dropWhile :: (Char -> Bool) -> T.Text -> T.Text
unit_dropWhile = "de" ... dropWhile (<= 'c') "abcde"

--   -- note change to increase safety
--   head :: T.Text -> Maybe Char
unit_head = Just 'a' ... head "abc"

-- last :: T.Text -> Maybe Char
unit_last =  Just 'c' ... last "abc"

--   tail :: T.Text -> Maybe T.Text
unit_tail = Just "bc" ... tail "abc"

--   init :: T.Text -> Maybe T.Text
unit_init = Just "ab" ... init "abc"

--   maximum :: (Ord Char) => T.Text -> Maybe Char
unit_maximum = Just 'c' ... maximum "abc"

--   minimum :: (Ord Char) => T.Text -> Maybe Char 
unit_minimum = Just 'a' ... minimum "abc"

--   -- no safe version
--   unsafeIndex :: T.Text -> Int -> Char
unit_unsafeIndex = 'c' ... unsafeIndex "abc" 2

--   null :: T.Text -> Bool
unit_null = chk $ null ""

-- any :: (Char -> Bool) -> T.Text -> Bool
unit_any = chk $ any (== 'a') "xyzab"

--   all :: (Char -> Bool) -> T.Text -> Bool
unit_all = chkFalse $ all (== 'a') "aaaab"

--   filter :: (Char -> Bool) -> T.Text -> T.Text 
unit_filter = "aaa" ... P.filter (== 'a') "afgrdavgwatyu" 

--   find :: (Char -> Bool) -> T.Text -> Maybe Char
unit_find = Just 'a' ... P.find (== 'a') "afgrdavgwatyu" 

--   foldl :: (b -> Char -> b) -> b -> T.Text -> b 
unit_foldl = 5 ... foldl (\a _ -> a + 1) 0 "12345"

-- foldlLazy :: (b -> Char -> b) -> b -> T.Text -> b 
unit_foldlLazy = 5 ... foldlLazy (\a _ -> a + 1) 0 "12345"

--   foldl1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldl1 = Just '5' ... foldl1 (\_ a -> a) "12345"
unit_foldl1_empty = Nothing ... foldl1 (\_ a -> a) ("" :: T.Text)

--   foldl1Lazy :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldl1Lazy = Just '5' ... foldl1Lazy (\_ a -> a) "12345"
unit_foldl1Lazy_empty = Nothing ... foldl1Lazy (\_ a -> a) ("" :: T.Text)

-- foldr :: (Char -> b -> b) -> b -> T.Text -> b 
unit_foldr = "abc" ... foldr cons "" "abc"

--   foldr1 :: (Char -> Char -> Char) -> T.Text -> Maybe Char
unit_foldr1 = Just 'a' ... foldr1 const "abc"
unit_foldr1_empty = Nothing ... foldr1 const ("" :: T.Text)

--   -- note difference so T.Text and list 
--   -- use functor instance for origonal implementation 
--   -- of list map
--   mapSimple :: (Char -> Char) -> T.Text -> T.Text
unit_mapSimple = "ABC" ... mapSimple C.toUpper "aBc"

--   zipSimple :: T.Text -> T.Text -> [(Char, Char)] 
unit_zipSimple = [('a', 'x'), ('b', 'y'), ('c', 'z')] ... zipSimple "abc" "xyz"

--   chunksOf :: Int -> T.Text -> [T.Text] 
unit_chunksOf = ["ab", "cd", "e"] ... chunksOf 2 "abcde"

--   empty :: T.Text
unit_empty = "" ... empty

--   unsnoc :: T.Text -> Maybe (T.Text, Char)
unit_unsnoc = Just ("abc", 'd') ... unsnoc "abcd"
 
--   partition :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_unit_partition = ("aaa", "bbyb") ... partition ('a' ==) "ababyab"

--   break :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_break = ("ab", "cde") ... break (== 'c') "abcde"

--   breakOn :: T.Text -> T.Text -> (T.Text, T.Text) 
unit_breakOn = ("ab", "cdefg" :: T.Text) ... breakOn "cd" "abcdefg"

--   breakOnEnd :: T.Text -> T.Text -> (T.Text, T.Text) 
unit_breakOnEnd = ("abcdefcd", "g" :: T.Text) ... breakOnEnd "cd" "abcdefcdg"

--   span :: (Char -> Bool) -> T.Text -> (T.Text, T.Text) 
unit_span = ("abcd", "efghij") ... span (< 'e') "abcdefghij"

--   takeEnd :: Int -> T.Text -> T.Text
unit_takeEnd = "fgh" ... takeEnd 3 "bcdefgh"

--   takeWhileEnd :: (Char -> Bool) -> T.Text -> T.Text
unit_takeWhileEnd = "dcba" ... takeWhileEnd (< 'e') "gfedcba"

--   splitOn :: T.Text -> T.Text -> [T.Text]
unit_splitOn = ["abcd", "ghi"] ... splitOn "ef" ("abcdefghi" :: T.Text)

--   split :: (Char -> Bool) -> T.Text -> [T.Text] 
unit_split = ["abcd", "fg" , "hi"] ... split (== 'e') "abcdefgehi"

--   dropWhileEnd :: (Char -> Bool) -> T.Text -> T.Text 
unit_dropWhileEnd = "abcdefg" ... dropWhileEnd (> 'g') "abcdefghi"

--   inits :: T.Text -> [T.Text]
unit_inits = ["","a","ab","abc","abcd","abcde","abcdef","abcdefg"] ... inits "abcdefg"

--   intercalate :: T.Text -> [T.Text] -> T.Text
unit_intercalate = "wasabcsawabcMe" ... intercalate "abc" ["was", "saw", "Me"]

--   intersperse :: Char -> T.Text -> T.Text
unit_intersperse = "axbxc" ... intersperse 'x' "abc"

--   isInfixOf :: T.Text -> T.Text -> Bool
unit_isInfixOf = chk $ isInfixOf "ef" ("abcdefgh" :: T.Text)

-- isPrefixOf :: T.Text -> T.Text -> Bool
unit_isPrefixOf = chk $ isPrefixOf "ab" ("abcdefgh" :: T.Text)

--   isSuffixOf :: T.Text -> T.Text -> Bool
unit_isSuffixOf = chk $ isSuffixOf "gh" ("abcdefgh" :: T.Text)

--   stripPrefix :: T.Text -> T.Text -> Maybe T.Text
unit_stripPrefix = Just "cdefgh" ... stripPrefix "ab" ("abcdefgh" :: T.Text)

--   stripSuffix :: T.Text -> T.Text -> Maybe T.Text
unit_stripSuffix = Just "abcdef" ... stripSuffix "gh" ("abcdefgh" :: T.Text)

--   tails :: T.Text -> [T.Text]
unit_tails =  ["abcdefgh","bcdefgh","cdefgh","defgh","efgh","fgh","gh","h",""] ... tails ("abcdefgh" :: T.Text)

--   transpose :: [T.Text] -> [T.Text]
unit_transpose = ["go","rr","ea","en","ng","e"] ... transpose ["green","orange"]

--   unfoldr :: (b -> Maybe (Char, b)) -> b -> T.Text
unit_unfoldr = "zyxwvutsrqponmlkjihgfedcba" ... unfoldr (\b -> if b < 97 then Nothing else Just (chr b, b-1)) 122

--   mapAccumL :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
unit_mapAccumL = (9, "bcdefghij") ... mapAccumL (\b c -> (b + 1, chr $ b + 98)) 0 "abcdefghi"

--   mapAccumR :: (b -> Char -> (b, Char)) -> b -> T.Text -> (b, T.Text) 
unit_mapAccumR =  (9,"iiiiiiiii") ... mapAccumR  (\b c -> (b + 1, chr . (+) b $ ord c)) 0 "abcdefghi"

--   drop :: Int -> T.Text -> T.Text
unit_drop = "defghi" ... drop 3 "abcdefghi"

--   length :: T.Text -> Int
unit_length = 9 ... length "abcdefghi"

--   scanlSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text
unit_scanlSimple = "aabcdef" ... scanlSimple (\a b -> if b > a then b else a) 'a' "abcdef"

--   scanl1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
unit_scanl1_empty = "" ... scanl1 (\a b -> if b > a then b else a)  ("" :: T.Text)
unit_scanl1 = "aaaaaa" ... scanl1 (\a b -> if b < a then b else a)  ("abcdef" :: T.Text)

--   scanrSimple :: (Char -> Char -> Char) -> Char -> T.Text -> T.Text 
unit_scanrSimple = "ffffffa" ... scanrSimple (\a b -> if b > a then b else a) 'a' "abcdef"

--   scanr1 :: (Char -> Char -> Char) -> T.Text -> T.Text 
unit_scanr1_empty = "" ... scanr1 (\a b -> if b > a then b else a)  ("" :: T.Text)
unit_scanr1 = "ffffff" ... scanr1 (\a b -> if b > a then b else a)  ("abcdef" :: T.Text)

--   splitAt :: Int -> T.Text -> (T.Text, T.Text)
unit_splitAt_outOfBounds = ("abcde", "") ... splitAt 10 "abcde"
unit_splitAt = ("ab", "cde") ... splitAt 2 "abcde"

--   take :: Int -> T.Text -> T.Text
unit_take = "abc" ... take 3 "abcderfdfdfds"

--   takeWhile :: (Char -> Bool) -> T.Text -> T.Text
unit_takeWhile = "abcd" ... takeWhile (< 'e') "abcdefgh"

--   cons :: Char -> T.Text -> T.Text
unit_cons = "abcde" ... cons 'a' "bcde"

--   snoc :: T.Text -> Char -> T.Text
unit_snoc = "abcde" ... snoc "abcd" 'e'

--   uncons :: T.Text -> Maybe (Char, T.Text) 
unit_uncons = Just ('a', "bcde") ... uncons "abcde"

--   zipWithSimple :: (Char -> Char -> Char) -> T.Text -> T.Text -> T.Text
unit_zipWithSimple = "cccde" ... zipWith (\a b -> if b > a then b else a)  "abcde" "cccccccccccc"