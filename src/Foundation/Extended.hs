module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , module Path.Extended
  , module Path.IO.Extended
  , module Foundation.Extended.Truthy
  , module Foundation.Extended.Stringy
  , module THEx
  , module Data.Discrimination
  , Stringy(..)
  , count
  , safeHead
  , firstDuplicate
  , eitherf
) where

import           Control.Monad.Catch
import           Data.Discrimination
import qualified Data.List                           as L
import qualified Data.Maybe                          as M
import           Debug.Trace.Extended
import           Foundation                          hiding (not, (&&), (||))
import           Foundation.Collection
import           Foundation.Extended.Stringy
import           Foundation.Extended.Truthy
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Path.IO.Extended
import qualified Prelude                             as P

count :: (Foldable collection, Truthy b, Additive a, P.Num a) => (Element collection -> b) -> collection -> a
count p = foldl' (\n x -> p x ? n + 1 $ n) 0

safeHead :: [a]-> Maybe a
safeHead = M.listToMaybe

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> length l > 1) (group xs) >>= safeHead

eitherf :: Either a b -> (a -> c) -> (b -> c) -> c
eitherf e lf rf = either lf rf e
