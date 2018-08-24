module Test.Hedgehog.Extended (
    module Hedgehog
  , module Internal.Test.Hedgehog.Gen.Extended
  , module Hedgehog.Range
  , discardGenerator
  , preludeString
  , Test.Hedgehog.Extended.string
  ) where

import           Foundation.Extended
import           Hedgehog
import           Hedgehog.Gen                        as GenFull
import           Hedgehog.Range
import           Internal.Test.Hedgehog.Gen.Extended
import qualified Prelude

-- discard conflicts with Hedgehog.discard
discardGenerator :: MonadGen m => m a
discardGenerator = GenFull.discard

-- switch string to generate Foundation.String
preludeString :: MonadGen m => Range Int -> m Char -> m Prelude.String
preludeString = GenFull.string

string :: MonadGen m => Range Int -> m Char -> m String
string range = (toStr <$>) <$> GenFull.string range
