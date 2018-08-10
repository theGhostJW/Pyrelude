
module HUnitTest where

--import           Foundation
-- import           Hedgehog
-- import qualified Hedgehog.Gen              as Gen
-- import qualified Hedgehog.Range            as Range
-- import qualified Prelude
-- import           Test.Tasty.Hedgehog       ()
import           Test.Tasty.HUnit.Extended


unit_chkEq = chkEq 1 1

unit_chkContains = chkContains "cool wor" "hello cool world"
