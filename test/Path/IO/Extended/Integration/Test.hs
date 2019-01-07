{-# LANGUAGE QuasiQuotes #-}

module Path.IO.Extended.Integration.Test where


import           Data.Either.Combinators
import           Foundation.Extended
import           Path.IO.Extended
import           Paths_principled_extended
import           Test.Extended


testDir = subDirFromBaseDir (parseAbsDir =<< getBinDir) [reldir|test|]

unit_subDirFromBaseDir_finds_test_dir :: Assertion
unit_subDirFromBaseDir_finds_test_dir =
  do
    dir <- testDir
    let
      exSuffix = "\\test\\"
      filePth = toStr $ toFilePath (fromRight' dir)
    chk' (exSuffix <> " is not a suffix of actual: " <> filePth) $ exSuffix `isSuffixOf` filePth

-- unit_chkEq2 = chkEq "" =<< getLibDir
