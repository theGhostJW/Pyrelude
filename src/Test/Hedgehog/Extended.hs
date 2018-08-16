module Test.Hedgehog.Extended (
    module Hedgehog
  , module Test.Hedgehog.Internal
  , module Hedgehog.Range
  , discardGenerator
  , preludeString
  , Test.Hedgehog.Extended.string

  ) where

import           Foundation.Extended
import           Hedgehog
import           Hedgehog.Gen           as GenFull
import           Hedgehog.Range
import qualified Prelude
import           Test.Hedgehog.Internal

discardGenerator :: MonadGen m => m a
discardGenerator = GenFull.discard

preludeString :: MonadGen m => Range Int -> m Char -> m Prelude.String
preludeString = GenFull.string

string :: MonadGen m => Range Int -> m Char -> m String
string range = (toStr <$>) <$> GenFull.string range
