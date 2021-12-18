module Pyrelude (
  module P
  , module Data.Maybe
  , module Data.List.Extra
  , module Data.Text.Encoding
  , module Data.Time
  , module Chronos
  , module EncodeError
  , module Control.Monad.Catch
  , module Debug.Trace.Extended
  , module Path.Extended
  , module Data.Either.Combinators
  , module Listy
  , module THEx
  , module Stringy
  , module System.Locale
  , module Text
  , module Ternary
  , module Control.Monad.Extra
  , module Fmt
  , sec -- second from Chronos - resolves name conflict with Base
  , (....) -- ~ chronos ...
  , singletonInterval
  , count
  , countValues
  , firstDuplicate
  , firstJust
  , firstJustf
  , eitherf
  , maybef
  , enumList
  , encodeErrorReplace
  , txt
  , txtPretty
  , groupD 
  , unlessJust
  , uu
) where


import           Control.Monad.Catch
import Chronos hiding (second, 
    singleton, 
    (...) {-  clashes with pyrethrym test equality check ... -} ) 
import qualified Chronos as Chron 
import Data.Time (TimeZone(..), utc)
import  qualified  BasePrelude as PAll
import qualified  Data.List.Extra as ListExtra
import           BasePrelude as P hiding (
   -- clashes with log in pyrethrym reexport as log10
   log,

   -- hiding String et. al. -- favouring Text
   String, lines, words, unlines, unwords, readFile, writeFile,


   -- favouring listy
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
   singleton,
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
   catchIOError, hPutStrLn,
   
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
      singleton, -- in favour of list
      take,
      takeEnd,
      takeWhile,
      takeWhileEnd,
      uncons,
      zipWith
  ) 
import           Data.Discrimination as D
import BasePrelude as B  hiding (singleton)
import           Data.Either.Combinators
import qualified Data.List                           as L hiding (singleton)
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
import qualified Data.Map.Strict as M
import Text.Show.Pretty as PP
import Fmt
import System.Locale (defaultTimeLocale)

import Data.List.Extra hiding (
                                lower,
                                upper,
                                singleton,
                                trim,
                                trimStart,
                                trimEnd,
                                word1,
                                line1,
                                escapeHTML,
                                escapeJSON,
                                unescapeHTML,
                                unescapeJSON,
                                firstJust,
                                --  in Listy
                                group, 
                                foldl1,
                                concat, 
                                concatMap,
                                groupBy,
                                reverse, 
                                dropWhile,
                                head, 
                                last, 
                                init,
                                maximum,
                                minimum,
                                null,
                                all,
                                filter,
                                find,
                                foldl',
                                foldr,
                                partition,
                                break,
                                span, 
                                dropWhileEnd,
                                inits,
                                isInfixOf,
                                isPrefixOf,
                                stripPrefix,
                                transpose,
                                unfoldr,
                                mapAccumR,
                                drop,
                                length,
                                scanl1,
                                scanr1,
                                take,
                                takeWhile,
                                breakOn,
                                chunksOf,
                                split,
                                splitOn,
                                unsnoc,
                                compareLength,
                                cons,
                                snoc,
                                stripSuffix,
                                takeWhileEnd,
                                lines,
                                words,
                                dropEnd,
                                replace,
                                unwords,
                                unlines,
                                takeEnd,
                                breakOnEnd,
                                uncons,
                                splitAt,
                                mapAccumL,
                                tails,
                                isSuffixOf,
                                intersperse,
                                intercalate,
                                foldr1,
                                any,
                                tail
                                )
import qualified Data.List.Extra as DE hiding (singleton)
  
import Control.Monad.Extra (whenJust) 

sec :: Timespan
sec = Chron.second

singletonInterval :: Time -> TimeInterval
singletonInterval = Chron.singleton

(....) :: Time -> Time -> TimeInterval
(....) = (Chron....)

firstJustf :: (a -> Maybe b) -> [a] -> Maybe b
firstJustf = DE.firstJust

firstJust :: [Maybe a] -> Maybe a
firstJust = DE.firstJust id

countValues :: Ord v => M.Map k v -> M.Map v Int
countValues = M.fromList . fmap ((\arr' -> (unsafeHead arr', Listy.length arr')) <$>) Listy.group . P.sort . M.elems


-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
unlessJust :: Applicative m => Maybe a -> m () -> m ()
unlessJust mg notingAction = maybe notingAction (const $ pure ()) mg

-- todo: orphaned instance for text and move out of Listy
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

txtPretty :: Show a => a -> Text
txtPretty = toS . ppShow

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

head :: [a] -> Maybe a
head = safe B.head