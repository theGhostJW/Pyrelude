{-# LANGUAGE QuasiQuotes #-}

module PathIOExtendedIntegrationTest where


import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           PyrethrumExtras
import           System.IO.Error
import           PyrethrumExtras.Test
import qualified Data.Text as T
import PyrethrumExtras.IO
import Paths_pyrethrum_extras
import BasePrelude.Operators ((=<<))
import BasePrelude ((<>), (<$>), ($))

testDir = subDirFromBaseDir (parseAbsDir =<< getBinDir) [reldir|test|]
baseDir = ((</> [reldir|Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir
invalidBaseDir = ((</> [reldir|Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir

chkSuffix :: T.Text -> Path a Dir -> Assertion
chkSuffix sfx dir =
  let
    filePth = toS $ toFilePath dir
  in
    chk' (sfx <> " is not a suffix of actual: " <> filePth) $ sfx `T.isSuffixOf` filePth

-- TODO - reinstate
-- unit_subDirFromBaseDir_finds_test_dir :: Assertion
-- unit_subDirFromBaseDir_finds_test_dir =
--   do
--     dir <- testDir
--     dir & either
--       (\l -> chkFail $ "testDir returned Left: " <> show l)
--       (chkSuffix "\\test\\")

-- unit_subDirFromBaseDir_finds_correct_temp :: Assertion
-- unit_subDirFromBaseDir_finds_correct_temp =
--     do
--       base <- baseDir
--       dir <- subDirFromBaseDir (eitherToError base) [reldir|temp|]
--       dir & either
--         (\l -> chkFail $ "testDir returned Left: " <> show l)
--         (chkSuffix "\\subFolder\\subSubFolder\\temp\\")
