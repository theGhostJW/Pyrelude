module Path.IO.Extended (
  module Path.IO
  , hasSubDir
  , readFile
  , readFileByteString
  , readFileUTF8
  , writeFile
  , writeFileUTF8
  , StrictReadResult
  , StrictReadError
  , seekDirUp
  , subDirFromBaseDir
) where

import qualified Data.ByteString                     as ByteString
import           Foundation
import           Foundation.Compat.ByteString        as CompatByteString
import           Foundation.Extended.Truthy
import           Foundation.Monad                    as FM
import           Foundation.String                   as String
import           GHC.IO.Exception
import           Path.Extended
import           Path.IO
import           System.IO.Error
import           Foundation.Extended.Stringy
import           Debug.Trace.Extended
import           Control.Monad.Catch as C


data StrictReadError =  IOFailure IOError
                             | StringValidationFailure  {
                                validationFailure :: ValidationFailure,
                                remainder :: UArray Word8
                             }
                             | IncompleteRead {remainder :: UArray Word8} -- should never happen as strict
                             deriving Show

type StrictReadResult = Either StrictReadError String

readFileByteString :: MonadIO m => Path a File -> m ByteString.ByteString
readFileByteString path = FM.liftIO $ ByteString.readFile $ toFilePath path

readFile :: (MonadIO m, C.MonadCatch m) => String.Encoding -> Path a File -> m StrictReadResult
readFile encoding path = let
                           toReadResult :: (String, Maybe String.ValidationFailure, UArray Word8) -> StrictReadResult
                           toReadResult (s, m, uw) = case m of
                                                       Just v -> Left $ StringValidationFailure v uw
                                                       Nothing -> null uw ?
                                                                    Right s $
                                                                    Left $ IncompleteRead uw
                          in
                            handleIOError
                              (pure . Left . IOFailure)
                              $ toReadResult . String.fromBytes encoding . fromByteString <$> readFileByteString path

readFileUTF8 :: (MonadIO m, C.MonadCatch m) => Path a File -> m StrictReadResult
readFileUTF8 = readFile UTF8

writeFile :: MonadIO m => String.Encoding -> Path a File -> String -> m ()
writeFile encoding path content = FM.liftIO $ ByteString.writeFile (toFilePath path) (toByteString $ toBytes encoding content)

writeFileUTF8 :: MonadIO m => Path a File -> String -> m ()
writeFileUTF8 = writeFile UTF8

notExistError :: String  -> Either IOError (Path a t)
notExistError errMsg = Left (mkIOError doesNotExistErrorType (toCharList errMsg) Nothing Nothing)

seekDirUp :: forall m a. Monad m => String -> Path a Dir -> (Path a Dir -> m Bool) -> m (Either IOError (Path a Dir))
seekDirUp errLabel dir prd =
  let
    thisParent :: Path a Dir
    thisParent = parent dir

    atBase :: Bool
    atBase = thisParent == dir

    seekDir' :: Path a Dir -> (Path a Dir -> m Bool) -> m (Either IOError (Path a Dir))
    seekDir' = seekDirUp errLabel
  in
    do
       found <- prd dir
       found ? pure (Right dir) $
        atBase ?
          pure (notExistError errLabel) $
          seekDir' thisParent prd

hasSubDir :: MonadIO m => Path a Dir -> Path Rel Dir -> m Bool
hasSubDir parentDir subDir = doesDirExist (parentDir </> subDir)

subDirFromBaseDir :: MonadIO m => m (Path a Dir) -> Path Rel Dir -> m (Either IOError (Path a Dir))
subDirFromBaseDir dir subDir =
  do
    baseParent <- dir
    let
      errLbl :: String
      errLbl = "Seeking directorry: " <> toStr (toFilePath subDir) <> " out from " <> toStr (toFilePath baseParent)

      dirPred :: MonadIO m => Path a Dir ->  m Bool
      dirPred parentDir = hasSubDir parentDir subDir

    ((</> subDir) <$>) <$> seekDirUp errLbl baseParent dirPred
