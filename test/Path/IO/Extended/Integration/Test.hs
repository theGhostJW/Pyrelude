{-# LANGUAGE QuasiQuotes #-}

module Path.IO.Extended.Integration.Test where


import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Pyrelude
import           Paths_pyrelude
import           System.IO.Error
import           Pyrelude.Test
import qualified Data.Text as T

testDir = subDirFromBaseDir (parseAbsDir =<< getBinDir) [reldir|test|]
baseDir = ((</> [reldir|path\IO\Extended\Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir
invalidBaseDir = ((</> [reldir|path\IOExtended\Integration\testData\subFolder\subSubFolder\subSubFolder\base|] ) <$>) <$> testDir

chkSuffix :: Text -> Path a Dir -> Assertion
chkSuffix sfx dir =
  let
    filePth = toS $ toFilePath dir
  in
    chk' (sfx <> " is not a suffix of actual: " <> filePth) $ sfx `T.isSuffixOf` filePth

unit_subDirFromBaseDir_finds_test_dir :: Assertion
unit_subDirFromBaseDir_finds_test_dir =
  do
    dir <- testDir
    eitherf dir
      (\l -> chkFail $ "testDir returned Left: " <> show l)
      (chkSuffix "\\test\\")

unit_subDirFromBaseDir_finds_correct_temp :: Assertion
unit_subDirFromBaseDir_finds_correct_temp =
    do
      base <- baseDir
      dir <- subDirFromBaseDir (eitherToError base) [reldir|temp|]
      eitherf dir
        (\l -> chkFail $ "testDir returned Left: " <> show l)
        (chkSuffix "\\subFolder\\subSubFolder\\temp\\")

-- TODO: reinstate
-- unit_IOError_returned_from_invalid_read :: Assertion
-- unit_IOError_returned_from_invalid_read = do
--                                     rr <- readFileUTF8 [absfile|C:\idoNotExist\idonotexist.txt|]
--                                     chkLeft
--                                      (\case
--                                         IOFailure _ -> True
--                                         err -> False
--                                      )
--                                      rr
