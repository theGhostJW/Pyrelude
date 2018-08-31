module Path.IO.Extended (
  module Path.IO
  , readFile
  , readFileByteString
  , readFileUTF8
  , writeFile
  , writeFileUTF8
  , StrictReadFailure
  , StrictReadError
) where

import qualified Data.ByteString              as ByteString
import           Foundation
import           Foundation.Compat.ByteString as CompatByteString
import           Foundation.Monad
import           Foundation.String            as String
import           Path.Extended
import           Path.IO

data StrictReadFailure = Failure ValidationFailure
                             | IncompleteRead -- should never happen as is strict
                             deriving Show

data StrictReadError  = StrictReadError {
  error     :: StrictReadFailure,
  remainder :: UArray Word8
} deriving Show

type StrictReadResult = Either StrictReadError String

readFileByteString :: MonadIO m => Path a File -> m ByteString.ByteString
readFileByteString path = Foundation.Monad.liftIO $ ByteString.readFile $ toFilePath path

readFile :: (MonadIO m) => String.Encoding -> Path a File -> m StrictReadResult
readFile encoding path = let
                           toReadResult :: (String, Maybe String.ValidationFailure, UArray Word8) -> StrictReadResult
                           toReadResult (s, m, uw) = case m of
                                                       Just v -> Left $ StrictReadError (Failure v) uw
                                                       Nothing -> if null uw
                                                                  then Right s
                                                                  else Left $ StrictReadError IncompleteRead uw
                          in
                           toReadResult . String.fromBytes encoding . fromByteString <$> readFileByteString path

readFileUTF8 :: (MonadIO m) => String.Encoding -> Path a File -> m StrictReadResult
readFileUTF8 path = readFile UTF8

writeFile :: MonadIO m => String.Encoding -> Path a File -> String -> m ()
writeFile encoding path content = Foundation.Monad.liftIO $ ByteString.writeFile (toFilePath path) (toByteString $ toBytes encoding content)

writeFileUTF8 :: MonadIO m => Path a File -> String -> m ()
writeFileUTF8 = writeFile UTF8

-- writeFile
-- -- maybe include a non-lenient variant that throws exceptions or
-- -- returns an Either value on bad character encoding
-- writeFile :: MonadIO m => FilePath -> ByteString -> m ()
-- writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
-- writeFileUtf8 fp = writeFile fp . encodeUtf8
-- -- conduit, pipes, streaming, etc, can handle the too-large-for-memory
-- -- case
