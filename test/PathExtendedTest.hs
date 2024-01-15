
module PathExtendedTest where

import qualified Control.Monad.Catch     as C
import Data.Either.Combinators ( isLeft )
import System.Info (os)
import Path.Extended as P
    ( Path,
      toFilePath,
      PathException,
      parseRelDirSafe,
      parseAbsDirSafe,
      parseRelFileSafe,
      parseAbsFileSafe )
import Stringy as P ( ConvertString, toS )
import PyrethrumExtras.Test.Tasty.HUnit.Extended
    ( Assertion, chkFail, chkEq, chk )
import qualified Prelude as P
import Data.Text
import BasePrelude (Either, show, pure)
import BasePrelude.Operators ( ($), (<>) )

type PathParser ar fd = forall m s. (C.MonadCatch m, ConvertString s P.String) => s -> m (Either PathException (Path ar fd))


chkInvalid ::  PathParser ar fd -> Text -> Text -> Assertion
chkInvalid parser parseTargetWin parseTargetLinux = 
    do
      rslt <- parser parseTarget
      chk $ isLeft rslt
    where
      parseTarget = case os of
             "mingw32" -> parseTargetWin
             "linux" -> parseTargetLinux
             _ -> P.error "Untested operating system"

chkParseValid :: PathParser ar fd -> Text -> Text -> Text -> Text -> Assertion
chkParseValid parser expectedWin parseTargetWin expectedLinux parseTargetLinux  = 
    case os of
      "mingw32" -> chkValid expectedWin parseTargetWin
      "linux" -> chkValid expectedLinux parseTargetLinux
      _ -> P.error "Untested operating system"
    where 
      chkValid expected parseTarget = 
        do
         rslt <- parser parseTarget
         P.either
            (\l -> chkFail $ "Parsing expected valid path resulted in error: " <> show l)
            (\pth -> chkEq expected (toS $ toFilePath pth))
            rslt


{-
module Main where

import System.FilePath ((</>), takeFileName)

-- Hard-coded base path for Linux
basePath :: FilePath
basePath = "/home/user/data"

-- Function to construct an absolute path
constructAbsolutePath :: FilePath -> FilePath
constructAbsolutePath relativePath = basePath </> relativePath

-- Function to construct a relative path
constructRelativePath :: FilePath -> FilePath
constructRelativePath filename = takeFileName basePath </> filename

main :: IO ()
main = do
  let relativePath = "subdirectory/file.txt"
  let absolutePath = constructAbsolutePath relativePath
  let relativeToBase = constructRelativePath "anotherfile.txt"

  putStrLn $ "Absolute Path: " ++ absolutePath
  putStrLn $ "Relative to Base Path: " ++ relativeToBase
-}

{-
Here's a quick clarification:

    Absolute path: Begins with a leading slash / and is specified from the root of the filesystem (e.g., /home/user/documents).

    Relative path: Is specified relative to the current working directory (e.g., subdirectory/file.txt).

So, for a relative path in Linux, you would use something like temp/ without the leading slash.
-}

unit_parseRelDirSafe_valid_nested = chkParseValid parseRelDirSafe "\\Temp\\Data\\" "\\Temp\\Data" "Temp/Data/" "Temp/Data"

unit_parseRelDirSafe_invalid = chkInvalid parseRelDirSafe "..\\Temp" "/Temp/Data/"
unit_parseRelDirSafe_invalid_empty = chkInvalid parseRelDirSafe """"

unit_parseAbsDirSafe_valid_nested = chkParseValid parseAbsDirSafe "D:\\Temp\\Data\\" "D:\\Temp\\Data" "/Temp/Data/" "/Temp/Data"

unit_parseAbsDirSafe_invalid = chkInvalid parseAbsDirSafe "\\Temp" "Temp/Data/"
unit_parseAbsDirSafe_invalid_empty = chkInvalid parseAbsDirSafe "" ""

unit_parseRelFileSafe_valid_nested = chkParseValid parseRelFileSafe "\\Temp\\Data\\MyFile.txt" "\\Temp\\Data\\MyFile.txt" "Temp/Data/File.txt" "Temp/Data/File.txt"

unit_parseRelFileSafe_invalid = chkInvalid parseRelFileSafe "..\\Temp\\MyFile.txt"  "/Temp/Data.txt"
unit_parseRelFileSafe_invalid_empty = chkInvalid parseRelFileSafe "" ""

unit_parseAbsFileSafe_valid_nested = chkParseValid parseAbsFileSafe "D:\\Temp\\Data\\MyFile.csv" "D:\\Temp\\Data\\MyFile.csv" "/Temp/Data/MyFile.csv"  "/Temp/Data/MyFile.csv"

unit_parseAbsFileSafe_invalid = chkInvalid parseAbsFileSafe "\\Temp\\MyFile.csv" "Temp/MyFile.csv"
unit_parseAbsFileSafe_invalid_empty = chkInvalid parseAbsFileSafe "" ""




-- main :: IO ()
-- main = do
--   let basePath = case os of
--                     "linux" -> "/path/on/linux/"
--                     "mingw32" -> "C:\\path\\on\\windows\\"
--                     _ -> error "Unsupported operating system"

--   putStrLn $ "Base path: " ++ basePath

  -- Now you can use basePath to construct your file paths or perform other actions based on the operating system.
