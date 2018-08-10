
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
import           Debug.Trace.Extended
import           Foundation.Extended
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

chkContains :: StringLike s => s -> s -> Assertion
chkContains needle hayStack = let
                                needleStr = debug' "Needle" $ toString needle
                                hayStackStr = debug' "Haystack" $ toString hayStack
                              in
                                assertBool (toList $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr)
                                $ needleStr `isInfixOf` hayStackStr

chk :: Bool -> Assertion
chk = assertBool "check failed"

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition
