
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
import qualified Prelude as P
import           Test.Tasty.HUnit       as HUnit hiding (Assertable,
                                                  AssertionPredicate, assert,
                                                  assertString)


safeLoad :: (Exception e) => (s -> IO a)  -> (Either e a -> Assertion) -> s -> Assertion
safeLoad stringLoader eitherConverter inputStr = eitherConverter =<< Control.Exception.Base.try (stringLoader inputStr)

chkFail :: ConvertString s P.String  => s -> Assertion
chkFail = assertFailure . toS

chkEq :: (Eq a, Show a) => a -> a -> Assertion
chkEq = (@=?)

(...) :: (Eq a, Show a)
                        => a -- ^ The expected value
                        -> a -- ^ The actual value
                        -> Assertion
(...) = (@=?)

chkContains :: ConvertString s String => s -> s -> Assertion
chkContains needle hayStack = let
                                needleStr = toS needle
                                hayStackStr = toS hayStack
                              in
                                chk' (toS $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr) $ needleStr `isInfixOf` (hayStackStr :: String)

chk :: Bool -> Assertion
chk = assertBool "check failed"

chk' :: String -> Bool -> Assertion
chk' errMsg = assertBool $ toS errMsg

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition

chkLeftContains :: (Show r, Show l, ConvertString s String) => s -> Either l r -> Assertion
chkLeftContains = chkLeftContains' show

chkLeftContains' :: forall r s s1 l. (Show r, ConvertString s String, ConvertString s1 String) => (l -> s1) -> s -> Either l r -> Assertion
chkLeftContains' leftToStr expectedText eth =
  let 
    expected :: String
    expected = toS expectedText
    
    errStr :: l -> String
    errStr = toS . leftToStr
  in
    eitherf eth
      (chkContains  expected . errStr)
      (\actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual)

chkLeft :: Show r => (l -> Bool) -> Either l r -> Assertion
chkLeft leftPred eth =
  case eth of
    Right actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual
    Left err     -> chk $ leftPred err
