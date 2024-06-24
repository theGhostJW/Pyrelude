module PyrethrumExtras.IO (
    module Path.IO.Extended,
    pp,
    putTxt,

) where

import Path.IO.Extended
import BasePrelude
import PyrethrumExtras (toS)
import Text.Show.Pretty (ppShow)
import Data.Text (Text)

prettyList :: (Show a) => a -> [String]
prettyList = lines . ppShow

putLines :: [String] -> IO ()
putLines = mapM_ putStrLn

pp :: (Show a) => a -> IO ()
pp = putLines . prettyList

putTxt :: Text -> IO ()
putTxt = putLines . lines . toS
