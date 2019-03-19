
module PyreludeDataTextHiddenTest where

import Pyrelude (Maybe(..))
import           Pyrelude.Test as T
import Pyrelude.Data.Text.Hidden 
import qualified Prelude as P

--- head ---
unit_head_null = Nothing ... head ""
unit_head_populated = Just 'a' ... head "abc"
unit_head_silgleton = Just 'b' ... head "b"

--- last ---
unit_last_null = Nothing ... last ""
unit_last_populated = Just 'c' ... last "abc"
unit_last_silgleton = Just 'z' ... last "z"

--- tail ---
unit_tail_null = Nothing ... tail ""
unit_tail_populated = Just "bc" ... tail "abc"
unit_tail_silgleton = Just "" ... tail "a"

--- init ---
unit_init_null = Nothing ... init ""
unit_init_populated = Just "ab" ... init "abc"

{-# ANN unit_init_silgleton ("HLint: ignore Use fmap" :: P.String) #-}
unit_init_silgleton = Just "" ... init "a"