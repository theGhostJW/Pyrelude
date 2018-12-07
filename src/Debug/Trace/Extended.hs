
module Debug.Trace.Extended (
  module Debug.Trace
  , debug
  , debug'
) where

import           Debug.Trace
import           Foundation

traceStr :: String -> a -> a
traceStr = trace . toList

debug :: Show a => a -> a
debug x = traceStr ("DEBUG: " <> show x) x

debug' :: Show a => String -> a -> a
debug' name x = traceStr ("DEBUG: " <> name <> ": " <> show x) x

debugf :: Show b => (a -> b) -> a -> a
debugf shower x = traceStr ("DEBUG: " <> show (shower x)) x

debugf' :: Show b => (a -> b) -> String -> a -> a
debugf' shower name x = traceStr ("DEBUG: " <> name <> ": " <> show (shower x)) x
