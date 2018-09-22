
module Language.Haskell.TH.Syntax.Extended where

import           Foundation
import           Language.Haskell.TH.Syntax as S


-- https://stackoverflow.com/a/5679470/5589037
moduleOf :: S.Name -> String
moduleOf =
  let
    dropLastToken :: String -> String
    dropLastToken = reverse . dropWhile (== '.') . dropWhile (/= '.') . reverse
  in
    dropLastToken . show
