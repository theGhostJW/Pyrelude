
module Test.Tasty.HUnit.Extended (
  module Test.Tasty.HUnit
  , safeLoad
  , chkEq
  , (?>)
  , chkContains
  , chk
  , chkFalse
) where

import           Control.Exception.Base
import qualified Data.List              as List
import           Foundation
import qualified Prelude
import           Test.Tasty.HUnit


safeLoad :: (Exception e) => (s -> IO a)  -> (Either e a -> Assertion) -> s -> Assertion
safeLoad stringLoader eitherConverter inputStr = eitherConverter =<< Control.Exception.Base.try (stringLoader inputStr)

chkEq :: (Eq a, Show a) => a -> a -> Assertion
chkEq = (@=?)

(?>) :: (Eq a, Show a)
                        => a -- ^ The expected value
                        -> a -- ^ The actual value
                        -> Assertion
(?>) = (@=?)

chkContains :: Show s => s -> s -> Assertion
chkContains needle hayStack = let
                                needleStr = Prelude.show needle
                                hayStackStr = Prelude.show hayStack
                              in
                                assertBool (toList $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr)
                                $ needleStr `List.isInfixOf` hayStackStr

chk :: Foundation.Bool -> Assertion
chk = assertBool "check failed"

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition
