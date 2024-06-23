module Stringy (
  module Data.String.Encode,
  toS,
  txt,
  txtShow,
  last,
  init,
  replaceFirst,
) where

import Data.String.Encode 
import BasePrelude (Show (..), (.), Maybe(..), Char)
import Data.Text qualified as T 
import Data.Text (Text) 
import Text.Show.Pretty (ppShow)

toS :: ConvertString a b => a -> b
toS = convertString

-- equivalent of show for text
txt :: Show a => a -> Text
txt = toS . ppShow

txtShow :: Show a => a -> Text
txtShow = toS . show


safet :: (Text -> b) -> Text -> Maybe b
safet unsafef t = 
  T.null t ? Nothing $ Just (unsafef t)
                                                            
last:: Text -> Maybe Char
last = safet T.last  

init :: Text -> Maybe Text
init = safet T.init


--  https://stackoverflow.com/questions/14922070/haskell-use-data-text-replace-to-replace-only-the-first-occurrence-of-a-text-va
replaceFirst :: Text -- ^ needle
                  -> Text -- ^ replacement
                  -> Text -- ^ haystack
                  -> Text
replaceFirst needle replacement haystack
    | T.null back = haystack 
    | otherwise = T.concat [front, replacement, T.drop (T.length needle) back] 
      where
        (front, back) = breakOn needle haystack