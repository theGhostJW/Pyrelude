module Foundation.Extended.Stringy where

import qualified Data.Bool  as B
import           Foundation
import Data.String.Encode 
import Foundation.Extended.ConvertStringOrphans

toS :: ConvertString a b => a -> b
toS = convertString

