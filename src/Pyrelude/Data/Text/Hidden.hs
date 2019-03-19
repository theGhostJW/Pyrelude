module Pyrelude.Data.Text.Hidden (
  module T
  , Pyrelude.Data.Text.Hidden.head
  , Pyrelude.Data.Text.Hidden.last
  , Pyrelude.Data.Text.Hidden.tail
  , Pyrelude.Data.Text.Hidden.init
) where 

import Data.Text as T (
  group, 
  
  reverse, dropWhile, concat, filter, map, zip, foldr, empty, foldl, foldl', 
  foldl1, foldr1, maximum, minimum, null, all, any, concatMap, find, partition,
  break, dropWhileEnd, findIndex, groupBy, inits, intercalate, intersperse, isInfixOf,
  isPrefixOf, isSuffixOf, stripPrefix, tails, transpose, unfoldr, mapAccumL, mapAccumR,
  drop, foldl1', replicate, length, scanl, scanl1, scanr, scanr1,
  span, splitAt, take, takeWhile, uncons, zipWith, index, 
  
  count
  )

import Pyrelude
import qualified Data.Text as QT 

safe :: (Text -> Char) -> Text -> Maybe Char
safe f l  = T.null l ? Nothing $ Just $ f l

safeLst :: (Text -> Text) -> Text -> Maybe Text
safeLst f l  = T.null l ? Nothing $ Just $ f l

head :: Text -> Maybe Char
head = safe QT.head

last :: Text -> Maybe Char
last = safe QT.last

tail :: Text -> Maybe Text
tail = safeLst QT.tail

init :: Text -> Maybe Text
init = safeLst QT.init