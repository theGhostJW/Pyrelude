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

import qualified Control.Monad.Catch                     as C
import           Foundation.Extended.Internal.StringLike
import           Path
import           Prelude                                 as P

type AbsDir = Path Abs Dir
type RelDir = Path Rel Dir
type RelFile = Path Rel File
type AbsFile = Path Abs File

safeParsePath :: (C.MonadCatch m, StringLike s) => (P.String -> m (Path ra df) ) -> s -> m (Either PathException (Path ra df))
safeParsePath parser subDirName = C.try $ parser $ toCharList subDirName

parseRelDirSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException RelDir)
parseRelDirSafe = safeParsePath parseRelDir

parseAbsDirSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException AbsDir)
parseAbsDirSafe = safeParsePath parseAbsDir

parseRelFileSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException RelFile)
parseRelFileSafe = safeParsePath parseRelFile

parseAbsFileSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException AbsFile)
parseAbsFileSafe = safeParsePath parseAbsFile
