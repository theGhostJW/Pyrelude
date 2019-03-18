
module Language.Haskell.TH.Syntax.Extended (
 module S,
 moduleOf
)
 where

import           BasePrelude as B
import           Data.Text hiding (reverse, dropWhile)
import           Language.Haskell.TH.Syntax as S
import           Stringy


-- https://stackoverflow.com/a/5679470/5589037
moduleOf :: S.Name -> Text
moduleOf =
  let
    dropLastToken :: String -> String
    dropLastToken = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse
  in
    toS . dropLastToken . show
