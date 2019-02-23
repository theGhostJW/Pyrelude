
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation.Extended.ConvertStringOrphans where

import Data.String.Encode
import Foundation
import Data.Text
import qualified Foundation.Compat.Text as C
import qualified Prelude as P

instance ConvertString String String where
  convertString = id

instance ConvertString String Text where
  convertString = C.toText

instance ConvertString String P.String where
  convertString = toList

instance ConvertString Text String where
  convertString = C.fromText

instance ConvertString P.String String where
  convertString = fromList

