
module Debug.Trace.Extended (
   debug
  , debug'
  , debugf
  , debugf'
  , debugPrint
) where

import           BasePrelude

debugPrint :: String -> a -> a
debugPrint s = trace (toList ("DEBUG: " <> s))

debug :: Show a => a -> a
debug x = debugPrint (show x) x

debug' :: Show a => String -> a -> a
debug' name x = debugPrint (name <> ": " <> show x) x

debugf :: Show b => (a -> b) -> a -> a
debugf shower x = debugPrint (show (shower x)) x

debugf' :: Show b => (a -> b) -> String -> a -> a
debugf' shower name x = debugPrint (name <> ": " <> show (shower x)) x