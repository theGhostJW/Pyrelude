module Path.Extended (
  module Path,
  parseRelDirSafe,
  parseAbsDirSafe,
  parseRelFileSafe,
  parseAbsFileSafe
) where

import qualified Control.Monad.Catch                     as C
import           Foundation.Extended.Internal.StringLike
import           Path
import           Prelude                                 as P

safeParsePath :: (C.MonadCatch m, StringLike s) => (P.String -> m (Path ra df) ) -> s -> m (Either PathException (Path ra df))
safeParsePath parser subDirName = C.try $ parser $ toCharList subDirName

parseRelDirSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException (Path Rel Dir))
parseRelDirSafe = safeParsePath parseRelDir

parseAbsDirSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException (Path Abs Dir))
parseAbsDirSafe = safeParsePath parseAbsDir

parseRelFileSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException (Path Rel File))
parseRelFileSafe = safeParsePath parseRelFile

parseAbsFileSafe :: (C.MonadCatch m, StringLike s) => s -> m (Either PathException (Path Abs File))
parseAbsFileSafe = safeParsePath parseAbsFile
