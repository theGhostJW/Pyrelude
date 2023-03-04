{-# LANGUAGE QuasiQuotes #-}

module PathIOExtendedIntegrationTest where


import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Pyrelude
import           Paths_pyrelude
import           System.IO.Error
import           Pyrelude.Test
import qualified Data.Text as T
import Pyrelude.IO

testDir = subDirFromBaseDir (parseAbsDir =<< getBinDir) [reldir|test|]
baseDir = ((</> [reldir|Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir
invalidBaseDir = ((</> [reldir|Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir

chkSuffix :: Text -> Path a Dir -> Assertion
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
--     eitherf dir
--       (\l -> chkFail $ "testDir returned Left: " <> show l)
--       (chkSuffix "\\test\\")

-- unit_subDirFromBaseDir_finds_correct_temp :: Assertion
-- unit_subDirFromBaseDir_finds_correct_temp =
--     do
--       base <- baseDir
--       dir <- subDirFromBaseDir (eitherToError base) [reldir|temp|]
--       eitherf dir
--         (\l -> chkFail $ "testDir returned Left: " <> show l)
--         (chkSuffix "\\subFolder\\subSubFolder\\temp\\")
