
module Test.Tasty.HUnit.Extended (
  module HUnit
  , safeLoad
  , chkEq
  , (...)
  , chkContains
  , chk
  , chk'
  , chkFail
  , chkFalse
  , chkLeft
  , chkLeftContains'
  , chkLeftContains
) where

import           Control.Exception.Base
import           Control.Monad
import           Foundation.Extended
import qualified Prelude
import           Test.Tasty.HUnit       as HUnit hiding (Assertable,
                                                  AssertionPredicate, assert,
                                                  assertString)


safeLoad :: (Exception e) => (s -> IO a)  -> (Either e a -> Assertion) -> s -> Assertion
safeLoad stringLoader eitherConverter inputStr = eitherConverter =<< Control.Exception.Base.try (stringLoader inputStr)

chkFail :: StringLike s => s -> Assertion
chkFail = assertFailure . toCharList

chkEq :: (Eq a, Show a) => a -> a -> Assertion
chkEq = (@=?)

(...) :: (Eq a, Show a)
                        => a -- ^ The expected value
                        -> a -- ^ The actual value
                        -> Assertion
(...) = (@=?)

chkContains :: StringLike s => s -> s -> Assertion
chkContains needle hayStack = let
                                needleStr = toStr needle
                                hayStackStr = toStr hayStack
                              in
                                assertBool (toList $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr)
                                $ needleStr `isInfixOf` hayStackStr

chk :: Bool -> Assertion
chk = assertBool "check failed"

chk' :: String -> Bool -> Assertion
chk' errMsg = assertBool $ toCharList errMsg

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition

chkLeftContains :: (Show r, Show l, StringLike s) => s -> Either l r -> Assertion
chkLeftContains = chkLeftContains' show

chkLeftContains' :: (Show r, StringLike s, StringLike s1) => (l -> s1) -> s -> Either l r -> Assertion
chkLeftContains' leftToTxt expectedText eth =
  case eth of
    Right actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual
    Left err     -> chkContains (toStr expectedText) (toStr $ leftToTxt err)

chkLeft :: Show r => (l -> Bool) -> Either l r -> Assertion
chkLeft leftPred eth =
  case eth of
    Right actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual
    Left err     -> chk $ leftPred err
