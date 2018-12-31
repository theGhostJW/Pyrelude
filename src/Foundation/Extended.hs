module Foundation.Extended (
  module Foundation
  , module Debug.Trace.Extended
  , module Path.Extended
  , module PathIO
  , module Foundation.Extended.Internal -- exports most of Path.IO and Foundation.Internal.Truthy
  , module THEx
  , module Data.Discrimination
  , StringLike(..)
  , count
  , safeHead
  , firstDuplicate
) where

import           Data.Discrimination
import           Data.Either
import qualified Data.List                           as L
import qualified Data.Maybe                          as M
import qualified Data.Text                           as Text
import           Debug.Trace.Extended
import           Foundation                          hiding (not, (&&), (||))
import           Foundation.Collection
import           Foundation.Compat.Text              as Compat
import           Foundation.Extended.Internal
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Path.IO.Extended                    as PathIO
import qualified Prelude                             as P

  -- fromString clashes with isString typeclass
class StringLike a where
  toStr :: a -> String
  fromStr :: String -> a
  toPreludeStr :: a -> P.String
  toText :: a -> Text.Text

instance StringLike String where
  fromStr = id
  toStr = id
  toPreludeStr = toList
  toText = Compat.toText

instance StringLike Text.Text where
  fromStr = Compat.toText
  toStr = fromText
  toPreludeStr = Text.unpack
  toText = id

instance StringLike P.String where
  fromStr = toList
  toStr = fromList
  toPreludeStr = id
  toText = Text.pack

count :: (Foldable collection, Truthy b, Additive a, P.Num a) => (Element collection -> b) -> collection -> a
count p = foldl' (\n x -> p x ? n + 1 $ n) 0

safeHead :: [a]-> Maybe a
safeHead = M.listToMaybe

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> length l > 1) (group xs) >>= safeHead
