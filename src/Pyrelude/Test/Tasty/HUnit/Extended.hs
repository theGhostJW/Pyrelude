
module Pyrelude.Test.Tasty.HUnit.Extended (
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

import Data.Text as T
import           Control.Exception.Base
import           Control.Monad
import           BasePrelude
import           Stringy
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

chkContains :: ConvertString s Text => s -> s -> Assertion
chkContains needle hayStack = let
                                needleStr = toS needle
                                hayStackStr = toS hayStack
                              in
                                chk' (toS $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr) $ needleStr `T.isInfixOf` (hayStackStr :: Text)

chk :: Bool -> Assertion
chk = assertBool "check failed"

chk' :: Text -> Bool -> Assertion
chk' errMsg = assertBool $ toS errMsg

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition

chkLeftContains :: (Show r, Show l, ConvertString s Text) => s -> Either l r -> Assertion
chkLeftContains = chkLeftContains' show

chkLeftContains' :: forall r s s1 l. (Show r, ConvertString s Text, ConvertString s1 Text) => (l -> s1) -> s -> Either l r -> Assertion
chkLeftContains' leftToStr expectedText eth =
  let 
    expected :: Text
    expected = toS expectedText
    
    errStr :: l -> Text
    errStr = toS . leftToStr
  in
    either 
      (chkContains  expected . errStr)
      (\actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual)
      eth

chkLeft :: Show r => (l -> Bool) -> Either l r -> Assertion
chkLeft leftPred =
  either 
    (chk . leftPred)
    (\actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual)
