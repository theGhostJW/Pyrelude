module Path.Extended (
  module Path,
  parseRelDirSafe,
  parseAbsDirSafe,
  parseRelFileSafe,
  parseAbsFileSafe,
  AbsDir,
  RelDir,
  RelFile,
  AbsFile
) where

import qualified Control.Monad.Catch         as C
import           Path hiding ((-<.> {- depricated -})) 
import           BasePrelude
import Data.String.Encode 
import Stringy

type AbsDir = Path Abs Dir
type RelDir = Path Rel Dir
type RelFile = Path Rel File
type AbsFile = Path Abs File

safeParsePath :: (C.MonadCatch m, ConvertString s String) => (String -> m (Path ra df) ) -> s -> m (Either PathException (Path ra df))
safeParsePath parser subDirName = C.try $ parser $ toS subDirName

parseRelDirSafe :: (C.MonadCatch m, ConvertString s String) => s -> m (Either PathException RelDir)
parseRelDirSafe = safeParsePath parseRelDir

parseAbsDirSafe :: (C.MonadCatch m, ConvertString s String) => s -> m (Either PathException AbsDir)
parseAbsDirSafe = safeParsePath parseAbsDir

parseRelFileSafe :: (C.MonadCatch m, ConvertString s String) => s -> m (Either PathException RelFile)
parseRelFileSafe = safeParsePath parseRelFile

parseAbsFileSafe :: (C.MonadCatch m, ConvertString s String) => s -> m (Either PathException AbsFile)
parseAbsFileSafe = safeParsePath parseAbsFile
