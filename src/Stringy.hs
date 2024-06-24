module Stringy (
  module Data.String.Encode,
  toS,
  txt,
  txtShow,
  lastChar,
  initTxt,
  replaceFirst,
) where

import BasePrelude (Char, Maybe (..), Show (..), otherwise, ($), (.))
import Data.String.Encode
import Data.Text (Text)
import Data.Text qualified as T
import Ternary ((?))
import Text.Show.Pretty (ppShow)

toS :: (ConvertString a b) => a -> b
toS = convertString

-- equivalent of show for text
txt :: (Show a) => a -> Text
txt = toS . ppShow

txtShow :: (Show a) => a -> Text
txtShow = toS . show

safet :: (Text -> b) -> Text -> Maybe b
safet unsafef t =
  T.null t ? Nothing $ Just (unsafef t)

lastChar :: Text -> Maybe Char
lastChar = safet T.last

initTxt :: Text -> Maybe Text
initTxt = safet T.init

--  https://stackoverflow.com/questions/14922070/haskell-use-data-text-replace-to-replace-only-the-first-occurrence-of-a-text-va
replaceFirst ::
  -- | needle
  Text ->
  -- | replacement
  Text ->
  -- | haystack
  Text ->
  Text
replaceFirst needle replacement haystack
  | T.null back = haystack
  | otherwise = T.concat [front, replacement, T.drop (T.length needle) back]
 where
  (front, back) = T.breakOn needle haystack