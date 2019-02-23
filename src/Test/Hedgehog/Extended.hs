
module Test.Hedgehog.Extended (
    module Hedgehog
  , module Gen 
  , module Hedgehog.Range
  , discardGenerator
  , charList
  , Test.Hedgehog.Extended.string
  ) where

import           Foundation.Extended
import           Hedgehog
import           Hedgehog.Gen        as GenFull
import qualified Hedgehog.Gen        as Gen hiding (constant, discard, string)
import           Hedgehog.Range
import qualified Prelude

{-# ANN module "HLint: Unnecessary hiding" #-}

-- discard conflicts with Hedgehog.discard
discardGenerator :: MonadGen m => m a
discardGenerator = GenFull.discard

-- switch string to generate Foundation.String
charList :: MonadGen m => Range Int -> m Char -> m Prelude.String
charList = GenFull.string

string :: MonadGen m => Range Int -> m Char -> m String
string range = (toS <$>) <$> GenFull.string range
