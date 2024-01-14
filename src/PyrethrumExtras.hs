module PyrethrumExtras (
  module Control.Monad.Catch
  , module Debug.Trace.Extended
  , module Path.Extended
  , module THEx
  , module Stringy
  , module Ternary
  , PyrethrumExtras.count
  , countValues
  , firstDuplicate
  , firstJust
  , firstJustf
  , enumList
  , txt
  , txtPretty
  , groupD 
  , unlessJust
  , uu
) where


import           Control.Monad.Catch
import  qualified  BasePrelude as PAll
import qualified  Data.List.Extra as ListExtra

import           Data.Discrimination as D
import Data.Text
import Text.Show.Pretty as PP
import BasePrelude as B  hiding (singleton)
import           Data.Either.Combinators
import qualified Data.List                           as L hiding (singleton)
import           Data.Maybe
import           Debug.Trace.Extended
import           Language.Haskell.TH.Syntax.Extended as THEx (moduleOf)
import           Path.Extended
import           Data.Text.Encoding
import Stringy
import Ternary
import qualified Data.Map.Strict as M
import System.Locale (defaultTimeLocale)
  
import Control.Monad.Extra (whenJust) 
import Data.Foldable (length)
import Data.List (head)

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

-- undefined in less keystrokes
uu :: forall a. a
uu = error "Not implemented"

-- equivalent of show for text
txt :: Show a => a -> Text
txt = toS . show

txtPretty :: Show a => a -> Text
txtPretty = toS . ppShow

count :: (Foldable f, Num n) => (a -> Bool) -> f a -> n
count p = PAll.foldl' (\n x -> p x ? n + 1 $ n) 0

firstDuplicate :: Grouping a => [a] -> Maybe a
firstDuplicate xs = L.find (\l -> B.length l > 1) (D.group xs) 
                      >>= \case 
                            [] -> Nothing 
                            x:_ -> Just x

enumList :: Enum a => [a]
enumList = enumFrom $ toEnum 0