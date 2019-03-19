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

import           BasePrelude as P
import  Data.ByteString as B (ByteString, readFile) 
import           Path.IO
import           Path
import           Stringy
import Data.Text as T
import qualified Data.Text.Encoding          as E
import Ternary
import Control.Monad.Catch

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
