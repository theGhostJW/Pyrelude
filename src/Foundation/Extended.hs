module Foundation.Extended (
  module Foundation
  , module Data.Maybe
  , module Control.Monad.Catch
  , module Debug.Trace.Extended
  , module Path.Extended
  , module Path.IO.Extended
  , module Data.Discrimination
  , module Data.Either.Combinators
  , module THEx
  , module Foundation.Extended.Truthy
  , module Foundation.Extended.Stringy
  , module Data.String.Encode
  , count
  , safeHead
  , safeLast
  , firstDuplicate
  , eitherf
  , maybef
  , enumList
) where

import           Control.Monad.Catch
import           Data.Discrimination
import           Data.Either.Combinators
import qualified Data.List                           as L
import           Data.Maybe
import           Debug.Trace.Extended
import           Foundation                          hiding (not, (&&), (||))
import           Foundation.Collection
import           Foundation.Extended.Stringy
import           Foundation.Extended.Truthy
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Path.IO.Extended
import qualified Prelude                             as P
import Foundation.Extended.ConvertStringOrphans
import Data.String.Encode

count :: (Foldable collection, Truthy b, Additive a, P.Num a) => (Element collection -> b) -> collection -> a
count p = foldl' (\n x -> p x ? n + 1 $ n) 0

safeHead :: [a]-> Maybe a
safeHead = listToMaybe

safeLast :: [a]-> Maybe a
safeLast l = L.null l ? Nothing $ Just $ L.last l

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> length l > 1) (group xs) >>= safeHead

eitherf :: Either a b -> (a -> c) -> (b -> c) -> c
eitherf e lf rf = either lf rf e

maybef :: Maybe a -> b -> (a -> b) -> b
maybef m d f = maybe d f m

enumList :: Enum a => [a]
enumList = enumFrom $ toEnum 0

