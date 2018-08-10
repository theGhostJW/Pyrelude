
module Test.Tasty.HUnit.Extended (
  module Test.Tasty.HUnit
  , safeLoad
  , chkEq
  , (?>)
  , chkContains
  , chk
  , chkFalse
  , chkErrorContains'
  , chkErrorContains
) where

import           Control.Exception.Base
import           Control.Monad
import           Debug.Trace.Extended
import           Foundation.Extended
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

chkErrorContains :: (Show r, StringLike s) => (l -> s) -> s -> Either l r -> Assertion
chkErrorContains = chkErrorContainsPriv False

chkErrorContains' :: (Show r, StringLike s) => (l -> s) -> s -> Either l r -> Assertion
chkErrorContains' = chkErrorContainsPriv True

chkErrorContainsPriv   :: (Show r, StringLike s) =>  Bool  -> (l -> s) -> s -> Either l r -> Assertion
chkErrorContainsPriv wantLogging leftToTxt expectedText eth  =
  case eth of
    Right actual -> do
                      let actualInfo =  "actual is: " <> show actual
                      when wantLogging  $
                          Prelude.print actualInfo
                      assertFailure . toList $ "Error expected but no error generated: " <> actualInfo
    Left err     -> do
                      let errs = leftToTxt err
                      when wantLogging $
                          Prelude.print $ "Error Generated: " <> toString errs
                      chkContains expectedText errs
