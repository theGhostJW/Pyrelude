
module Debug.Trace.Extended (
   debug
  , debug'
  , debugf
  , debugf'
) where

import           BasePrelude
import    qualified       Data.Text as T
import Text.Show.Pretty as PP
import qualified Prelude as P


debugLbl :: T.Text
debugLbl = "DEBUG"

debug :: Show a => a -> a
debug = debug' debugLbl

debug' :: Show a => T.Text -> a -> a
debug' = debugf' id

debugf :: Show b => (a -> b) -> a -> a
debugf shower = debugf' shower debugLbl

debugf' :: Show b => (a -> b) -> T.Text -> a -> a
debugf' shower lbl expr = 
  let 
    lst = T.lines . T.pack . ppShow $ shower expr
  in
    unsafePerformIO $ do
      case lst of 
        [] -> traceIO $ T.unpack lbl
        [x] -> traceIO . T.unpack $ lbl <> ": " <> x
        ls@(x : xs) -> sequence_ $ traceIO . T.unpack <$> "--- " <> lbl <> " ---" : ls
      return expr