{-# LANGUAGE QuasiQuotes #-}

module Path.IO.Extended.Integration.Test where


import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Foundation.Extended
import           Path.IO.Extended
import           Paths_principled_extended
import           System.IO.Error
import           Test.Extended

testDir = subDirFromBaseDir (parseAbsDir =<< getBinDir) [reldir|test|]
baseDir = ((</> [reldir|path\IO\Extended\Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir

chkSuffix :: String -> Path a Dir -> Assertion
chkSuffix sfx dir =
  let
    filePth = toStr $ toFilePath dir
  in
    chk' (sfx <> " is not a suffix of actual: " <> filePth) $ sfx `isSuffixOf` filePth

unit_subDirFromBaseDir_finds_test_dir :: Assertion
unit_subDirFromBaseDir_finds_test_dir =
  do
    dir <- testDir
    either
      (\l -> chkFail $ "testDir returned Left: " <> show l)
      (chkSuffix "\\test\\")
      dir

unit_subDirFromBaseDir_finds_correct_temp :: Assertion
unit_subDirFromBaseDir_finds_correct_temp =
    do
      base <- baseDir
      dir <- subDirFromBaseDir (eitherToError base) [reldir|temp|]
      either
        (\l -> chkFail $ "testDir returned Left: " <> show l)
        (chkSuffix "\\subFolder\\subSubFolder\\temp\\")
        dir
