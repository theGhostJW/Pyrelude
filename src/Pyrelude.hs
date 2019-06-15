module Pyrelude (
  module P
  , module Data.Maybe
  , module Data.List.Extra
  , module Data.Text.Encoding
  , module EncodeError
  , module Control.Monad.Catch
  , module Debug.Trace.Extended
  , module Path.Extended
  , module Data.Either.Combinators
  , module Listy
  , module THEx
  , module Stringy
  , module Text
  , module Ternary
  , module Control.Monad.Extra
  , count
  , firstDuplicate
  , eitherf
  , maybef
  , enumList
  , encodeErrorReplace
  , txt
  , groupD 
  , unlessJust
  , uu
) where


import           Control.Monad.Catch
import  qualified  BasePrelude as PAll
import           BasePrelude as P hiding (
   -- clashes with log in pyrethrym reexport as log10
   log,

   -- hiding String et. al. -- favouring Text
   String, lines, words, unlines, unwords, readFile, writeFile,


   -- favourung listy
   foldl1,
   foldl1',
   concat,
   concatMap,
   groupBy,
   group,
   reverse,
   dropWhile,
   head,
   last,
   tail,
   init,
   maximum,
   minimum,
   null,
   any,
   all,
   filter,
   find,
   foldl,
   foldl',
   foldl1,
   foldr,
   foldr1,
   map,
   empty,
   partition,
   break,
   span,
   dropWhileEnd,
   inits,
   intercalate,
   intersperse,
   isInfixOf,
   isPrefixOf,
   isSuffixOf,
   stripPrefix,
   tails,
   transpose,
   unfoldr,
   mapAccumL,
   mapAccumR,
   drop,
   length,
   scanl1,
   scanr1,
   splitAt,
   take,
   takeWhile,
   uncons,
  

   -- Hidden because unlikely to be used an clashes with
   -- filter constructors in Pyretherum
   All, Last,

   -- hidng in favour of Control.Monad.Catch -- exceptions
   -- TODO: work this out make sure behaviour is the same
   Handler, catches, bracket, bracketOnError, bracket_, catchJust, finally, handle, handleJust,
   onException, try, tryJust, catch, mask, mask_, uninterruptibleMask, uninterruptibleMask_, 
   catchIOError, 
   
   (!!), -- use unsafeIndex

   -- favouring Data.Either.Combinators
   isLeft, fromRight, isRight, fromLeft,

   -- Favour Text
   toLower, toTitle, toUpper, 

   -- Favour Data.Text.IO
   appendFile, getContents, getLine, interact, putStr, putStrLn, 
     ) 
import Data.Text as Text hiding (
      breakOn,
      breakOnEnd,
      chunksOf,
      split,
      splitOn,
      count, -- use countText
      concat,
      concatMap,
      foldl1',
      foldl',
      groupBy,
      group,
      reverse,
      dropWhile,
      head,
      last,
      tail,
      init,
      index,
      maximum,
      minimum,
      null,
      any,
      all,
      filter,
      find,
      foldl,
      foldl1,
      foldr,
      foldr1,
      map,
      zip,
      empty,
      partition,
      break,
      span,
      dropWhileEnd,
      findIndex,
      inits,
      intercalate,
      intersperse,
      isInfixOf,
      isPrefixOf,
      isSuffixOf,
      unsnoc, 
      cons, 
      replicate,
      snoc,
      stripPrefix,
      stripSuffix,
      tails,
      transpose,
      unfoldr,
      mapAccumL,
      mapAccumR,
      drop,
      length,
      scanl,
      scanl1,
      scanr,
      scanr1,
      splitAt,
      take,
      takeEnd,
      takeWhile,
      takeWhileEnd,
      uncons,
      zipWith
  ) 
import           Data.Discrimination as D
import BasePrelude as B
import           Data.Either.Combinators
import qualified Data.List                           as L
import  Listy
import           Data.Maybe
import           Debug.Trace.Extended
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Data.Text.Encoding
import           Data.Text.Encoding.Error as EncodeError hiding (replace)
import           Data.Text.Encoding.Error (replace)
import Stringy
import Ternary
import Data.List.Extra (
      --- * Note string functions excluded
      --- * depricated for function excluded
      splitAtEnd, breakEnd, spanEnd,
      dropWhileEnd', 
      stripInfix, stripInfixEnd,
      dropPrefix, dropSuffix,
      wordsBy, linesBy,
      -- * Basics
      notNull, list, drop1, mconcatMap,
      -- * List operations
      groupSort, groupSortOn, groupSortBy,
      nubOrd, nubOrdBy, nubOrdOn,
      nubOn, groupOn, sortOn,
      nubSort, nubSortBy, nubSortOn,
      maximumOn, minimumOn,
      disjoint, allSame, anySame,
      repeatedly, firstJust,
      concatUnzip, concatUnzip3,
      zipFrom, zipWithFrom,
      merge, mergeBy
  )
import Control.Monad.Extra (whenJust) 


-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
unlessJust :: Applicative m => Maybe a -> m () -> m ()
unlessJust mg notingAction = maybe notingAction (const $ pure ()) mg

-- todo: orphanned instance for text and move out of Listy
groupD :: Grouping a => [a] -> [[a]] 
groupD  = D.group

log10 :: Floating a => a -> a
log10 = B.log

encodeErrorReplace :: b -> OnError a b 
encodeErrorReplace = Data.Text.Encoding.Error.replace

-- undefined in less keystrokes
uu :: forall a. a
uu = undefined

-- equivalent of show for text
txt :: Show a => a -> Text
txt = toS . show

count :: (Foldable f, Num n) => (a -> Bool) -> f a -> n
count p = PAll.foldl' (\n x -> p x ? n + 1 $ n) 0

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> B.length l > 1) (D.group xs) >>= Pyrelude.head

eitherf :: Either a b -> (a -> c) -> (b -> c) -> c
eitherf e lf rf = either lf rf e

maybef :: Maybe a -> b -> (a -> b) -> b
maybef m d f = maybe d f m

enumList :: Enum a => [a]
enumList = enumFrom $ toEnum 0

safe :: ([a] -> a) -> [a] -> Maybe a
safe f l  = L.null l ? Nothing $ Just $ f l

-- safeLst :: ([a] -> [a]) -> [a] -> Maybe [a]
-- safeLst f l  = Listy.null l ? Nothing $ Just $ f l

head :: [a] -> Maybe a
head = safe B.head

-- last :: [a] -> Maybe a
-- last = safe B.last

-- tail :: [a] -> Maybe [a]
-- tail = safeLst B.tail

-- init :: [a] -> Maybe [a]
-- init = safeLst B.init