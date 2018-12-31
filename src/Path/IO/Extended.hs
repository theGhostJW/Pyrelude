module Path.IO.Extended (
  module Path.IO
  , readFile
  , readFileByteString
  , readFileUTF8
  , writeFile
  , writeFileUTF8
  , StrictReadResult
  , StrictReadFailure
  , StrictReadError
  , seekDirFromBase
  , seekInDir
) where

import qualified Data.ByteString                     as ByteString
import           Foundation
import           Foundation.Compat.ByteString        as CompatByteString
import           Foundation.Extended.Internal.Truthy
import           Foundation.Monad
import           Foundation.String                   as String
import           GHC.IO.Exception
import           Path.Extended
import           Path.IO
import           System.IO.Error

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

readFileUTF8 :: (MonadIO m) => Path a File -> m StrictReadResult
readFileUTF8 = readFile UTF8

writeFile :: MonadIO m => String.Encoding -> Path a File -> String -> m ()
writeFile encoding path content = Foundation.Monad.liftIO $ ByteString.writeFile (toFilePath path) (toByteString $ toBytes encoding content)

writeFileUTF8 :: MonadIO m => Path a File -> String -> m ()
writeFileUTF8 = writeFile UTF8

notExistError :: Path a t  -> Either IOError (Path a t)
notExistError missingFile = Left (mkIOError doesNotExistErrorType (toFilePath missingFile) Nothing Nothing)

-- seekDirFromBase :: (Monad m)  => m (Path a t) -> (Path a Dir  -> m Bool) -> m (Either IOError (Path a Dir))
-- seekDirFromBase base dirPredicate = do
--                                      path <- base
--                                      result <- seekInDir path dirPredicate
--                                      pure $ (result == "") ?
--                                                  notExistError "Directory not found" $
--                                                  Right result

seekInDir :: (Monad m) => Path a t -> (Path a Dir -> m Bool) -> m (Path a Dir)
seekInDir dir prd =
  let
    thisParent = parent dir
    atBase = parent == dir
  in
    do
       found <- prd dir
       found ? pure dir $
         atBase ? pure "" $
         seekInDir parent prd
