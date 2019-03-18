
module Test.Tasty.HUnit.Extended.Test where

import           Pyrelude.Test 

unit_chkEq = chkEq 1 1

unit_chkContains = chkContains "cool wor" "hello cool world"
