module Path.IO.Extended (
  module Path.IO
  , hasSubDir
  , seekDirUp
  , subDirFromBaseDir
  , openFile
) where

import BasePrelude as P
    ( ($),
      Eq((==)),
      Monad,
      Applicative(pure),
      Semigroup((<>)),
      Bool,
      Maybe(Nothing),
      Either(..),
      (<$>),
      doesNotExistErrorType,
      mkIOError,
      MonadIO,
      IOError, IO, String, putStrLn, (.) )
import           Path.IO
import Path ( toFilePath, (</>), parent, Path, Dir, Rel )
import Stringy ( toS, ConvertString )
import Data.Text as T ( Text )
import qualified Data.Text.Encoding          as E
import Ternary ( (?) )
import qualified System.IO as S
import PyrethrumExtras (AbsFile, catchIOError)

notExistError :: Text  -> Either IOError (Path a t)
notExistError errMsg = Left (mkIOError doesNotExistErrorType (toS errMsg) Nothing Nothing)

seekDirUp :: forall m a. Monad m => Text -> Path a Dir -> (Path a Dir -> m Bool) -> m (Either IOError (Path a Dir))
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
      errLbl :: Text
      errLbl = "Seeking directory: " <> toS (toFilePath subDir) <> " out from " <> toS (toFilePath baseParent)

      dirPred :: MonadIO m => Path a Dir ->  m Bool
      dirPred parentDir = hasSubDir parentDir subDir

    ((</> subDir) <$>) <$> seekDirUp errLbl baseParent dirPred

openFile :: AbsFile -> S.IOMode -> IO (Either IOError S.Handle)
openFile pth mode =
  catchIOError (Right <$> S.openFile (toFilePath pth) mode) (pure . Left)

