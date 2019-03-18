module Path.IO.Extended (
  module Path.IO
  , hasSubDir
  -- , readFile  -- use injected function
  -- , readFileByteString
  -- , readFileUTF8
 -- , writeFile
 -- , writeFileUTF8
 -- , StrictReadResult
 -- , StrictReadError(..)
  , seekDirUp
  , subDirFromBaseDir
) where

import           BasePrelude -- hiding (readFile, writeFile)
import qualified Data.ByteString                     as ByteString
import           Path.IO
import           Path
import           Stringy
import Data.Text as T
import qualified Data.Text.Encoding          as E
import Ternary


-- data StrictReadError =  IOFailure IOError
--                              | StringValidationFailure  {
--                                 validationFailure :: ValidationFailure,
--                                 remainder :: UArray Word8
--                              }
--                              | IncompleteRead {remainder :: UArray Word8} -- should never happen as strict
--                              deriving Show

-- type StrictReadResult = Either StrictReadError String

-- readFileByteString :: MonadIO m => Path a File -> m ByteString.ByteString
-- readFileByteString path = FM.liftIO $ ByteString.readFile $ toFilePath path

-- readFileUTF8 :: (MonadIO m, C.MonadCatch m) => String.Encoding -> Path a File -> m StrictReadResult
-- readFileUTF8 encoding path = let
--                          --  toReadResult :: (String, Maybe String.ValidationFailure, UArray Word8) -> StrictReadResult
--                            toReadResult (s, m, uw) = case m of
--                                                        Just v -> Left $ StringValidationFailure v uw
--                                                        Nothing -> null uw ?
--                                                                     Right s $
--                                                                     Left $ IncompleteRead uw
--                           in
--                             handleIOError
--                               (pure . Left . IOFailure)
--                               $ toReadResult $ E.decodeUtf8' <$> readFileByteString path
                        --      $ toReadResult . String.fromBytes encoding . fromByteString <$> readFileByteString path

-- from quickstep
-- myReadFile :: FilePath -> IO (Either IOError Text)
-- myReadFile fullFilePath = do
--                             h    <- openFile fullFilePath IO.ReadMode
--                             bs   <- BS.hGetContents h
--                             pure $ pure $ TextEncoding.decodeUtf8 bs

-- readFileUTF8 :: (MonadIO m, C.MonadCatch m) => Path a File -> m StrictReadResult
-- readFileUTF8 = readFile UTF8

-- writeFile :: MonadIO m => String.Encoding -> Path a File -> String -> m ()
-- writeFile encoding path content = liftIO $ ByteString.writeFile (toFilePath path) (toByteString $ toBytes encoding content)

-- writeFileUTF8 :: MonadIO m => Path a File -> String -> m ()
-- writeFileUTF8 = writeFile UTF8

notExistError :: String  -> Either IOError (Path a t)
notExistError errMsg = Left (mkIOError doesNotExistErrorType (toS errMsg) Nothing Nothing)

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
      errLbl = "Seeking directorry: " <> toS (toFilePath subDir) <> " out from " <> toS (toFilePath baseParent)

      dirPred :: MonadIO m => Path a Dir ->  m Bool
      dirPred parentDir = hasSubDir parentDir subDir

    ((</> subDir) <$>) <$> seekDirUp errLbl baseParent dirPred
