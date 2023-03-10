
module PyrethrumExtras.Test.Hedgehog.Extended (
    module Hedgehog
  , module Gen 
  , module Hedgehog.Range
  , discardGenerator
  ) where

import           BasePrelude
import           Hedgehog
import           Hedgehog.Gen        as GenFull
import qualified Hedgehog.Gen        as Gen hiding (constant, discard)
import           Hedgehog.Range
import qualified Prelude

{-# ANN module "HLint: Unnecessary hiding" #-}

-- discard conflicts with Hedgehog.discard
discardGenerator :: MonadGen m => m a
discardGenerator = GenFull.discard

