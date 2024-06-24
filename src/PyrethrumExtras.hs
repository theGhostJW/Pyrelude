module PyrethrumExtras (
  module Control.Monad.Catch
  , module Debug.Trace.Extended
  , module Path.Extended
  , module THEx
  , module Stringy
  , module Ternary
  , module Time
  , module Listy
  , module ListExtraExport
  , countValues
  , firstDuplicate
  , firstJust
  , firstJustf
  , groupD 
  , unlessJust
) where


import           Control.Monad.Catch
import qualified  Data.List.Extra as ListExtra
import qualified  Data.List.Extra as ListExtraExport (enumerate)

import Data.Discrimination as D ( group, Grouping )
import BasePrelude as B
    ( ($),
      Floating(log),
      Monad((>>=)),
      Functor(fmap),
      Ord((>)),
      Applicative(pure),
      Foldable(length),
      Int,
      Maybe(..),
      (<$>),
      maybe,
      const,
      error,
      Category((.), id) )
import qualified Data.List                           as L hiding (singleton)
import           Debug.Trace.Extended
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import Data.Text.Encoding ()
import Stringy
import Ternary
import Listy
import Time
import qualified Data.Map.Strict as M
  
import Data.Foldable (length)
import Data.List ()

firstJustf :: (a -> Maybe b) -> [a] -> Maybe b
firstJustf = ListExtra.firstJust

firstJust :: [Maybe a] -> Maybe a
firstJust = ListExtra.firstJust id

countValues :: Ord v => M.Map k v -> M.Map v Int
countValues = M.fromList . fmap ((\arr' -> (dodgyHead arr', Data.Foldable.length arr')) <$>) L.group . L.sort . M.elems

dodgyHead :: [a] -> a
dodgyHead = \case 
  [] -> error "dodgyHead: empty list"
  x:_ -> x

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
unlessJust :: Applicative m => Maybe a -> m () -> m ()
unlessJust mg notingAction = maybe notingAction (const $ pure ()) mg

-- todo: orphaned instance for text and move out of ListLike
groupD :: Grouping a => [a] -> [[a]] 
groupD  = D.group

log10 :: Floating a => a -> a
log10 = B.log

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> B.length l > 1) (D.group xs) 
                      >>= \case 
                            [] -> Nothing 
                            x:_ -> Just x