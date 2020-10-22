
module Debug.Trace.Extended (
   debug
  , debug'
  , debugf
  , debugf'
  
  , debug_
  , debug'_
  , debugf_
  , debugf'_
) where

import           BasePrelude
import    qualified       Data.Text as T
import Text.Show.Pretty as PP


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
        ls -> sequence_ $ traceIO . T.unpack <$> "--- " <> lbl <> " ---" : ls
      return expr

-- id functions to enable disabling debug messages --

debug'_ :: T.Text -> a -> a
debug'_ _ a = a

debug_ :: a -> a
debug_ = id

debugf'_ :: f -> T.Text -> a -> a
debugf'_ _ _ a = a

debugf_ :: (a -> b) -> a -> a
debugf_ _ a = a
