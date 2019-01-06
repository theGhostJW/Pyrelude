
module Test.Tasty.HUnit.Extended.Test where

import           Test.Tasty.HUnit.Extended


unit_chkEq = chkEq 1 1

unit_chkContains = chkContains "cool wor" "hello cool world"
