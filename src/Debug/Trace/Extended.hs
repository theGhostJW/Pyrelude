
module Debug.Trace.Extended (
   debug
  , debug'
  , debugf
  , debugf'
  , debugPrint
) where

import           BasePrelude
import    qualified       Data.Text as T

debugPrint :: T.Text -> a -> a
debugPrint s = trace (T.unpack ("DEBUG: " <> s))

debug :: Show a => a -> a
debug a = debugPrint (T.pack $ show a) a

label :: Show a => T.Text -> a -> T.Text
label name a = name <> ": " <> T.pack (show a)

debug' :: Show a => T.Text -> a -> a
debug' name a = debugPrint (label name a) a

debugf :: Show b => (a -> b) -> a -> a
debugf shower a = debugPrint (T.pack $ show (shower a)) a

debugf' :: Show b => (a -> b) -> T.Text -> a -> a
debugf' shower name a = debugPrint (label name $ shower a) a