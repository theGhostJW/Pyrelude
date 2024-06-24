module PyrethrumExtras.Test.Tasty.HUnit.Extended
  ( module HUnit,
    safeLoad,
    chkEq,
    chkEq',
    (...),
    chkContains,
    chk,
    chk',
    chkFail,
    chkFalse,
    chkLeft,
    chkLeftContains',
    chkLeftContains,
  )
where

import BasePrelude
import Test.Tasty.HUnit as HUnit hiding
  ( Assertable,
    AssertionPredicate,
    assert,
    assertString,
  )
import Prelude qualified as P
import Data.String.Encode 
import Data.Text qualified as T
import Control.Exception.Base qualified
import Stringy (toS)

safeLoad :: (Exception e) => (s -> IO a) -> (Either e a -> Assertion) -> s -> Assertion
safeLoad stringLoader eitherConverter inputStr = eitherConverter =<< Control.Exception.Base.try (stringLoader inputStr)

chkFail :: ConvertString s P.String => s -> Assertion
chkFail = assertFailure . toS

chkEq :: (Eq a, Show a) => a -> a -> Assertion
chkEq = (@=?)

chkEq' ::
  (Eq a, Show a, HasCallStack) =>
  -- | The message prefix
  T.Text ->
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
chkEq' t = assertEqual (toS t)

infixr 0 ...

(...) ::
  (Eq a, Show a) =>
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
(...) = (@=?)

chkContains :: ConvertString s T.Text => s -> s -> Assertion
chkContains needle hayStack =
  let needleStr = toS needle
      hayStackStr = toS hayStack
   in chk' (toS $ "substring not found: " <> needleStr <> "\n<<<IN>>>\n" <> hayStackStr) $ needleStr `T.isInfixOf` (hayStackStr :: T.Text)

chk :: Bool -> Assertion
chk = assertBool "check failed"

chk' :: T.Text -> Bool -> Assertion
chk' errMsg = assertBool $ toS errMsg

chkFalse :: Bool -> Assertion
chkFalse condition = chk $ not condition

chkLeftContains :: (Show r, Show l, ConvertString s T.Text ) => s -> Either l r -> Assertion
chkLeftContains = chkLeftContains' show

chkLeftContains' :: forall r s s1 l. (Show r, ConvertString s T.Text , ConvertString s1 T.Text ) => (l -> s1) -> s -> Either l r -> Assertion
chkLeftContains' leftToStr expectedText eth =
  let expected :: T.Text 
      expected = toS expectedText

      errStr :: l -> T.Text 
      errStr = toS . leftToStr
   in either
        (chkContains expected . errStr)
        (\actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual)
        eth

chkLeft :: Show r => (l -> Bool) -> Either l r -> Assertion
chkLeft leftPred =
  either
    (chk . leftPred)
    (\actual -> chkFail $ "Error expected but no error generated. Actual is: " <> show actual)
