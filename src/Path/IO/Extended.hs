module Path.IO.Extended (
  module Path.IO
  , readFile
  , writeFile
) where

import           Foundation
import           Path.Extended
import           Path.IO
import qualified Prelude

readFile :: Path a File -> IO String
readFile pth = fromList <$> Prelude.readFile (toFilePath pth)

writeFile :: (s -> Prelude.String) -> Path a File -> s -> Prelude.IO ()
writeFile stringifyer pth str = Prelude.writeFile (toFilePath pth) (stringifyer str)
