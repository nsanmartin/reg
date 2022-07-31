module Store
where

import System.Directory (getHomeDirectory)
import qualified System.IO.Strict as SIO

import Query

getRegfile :: IO FilePath
getRegfile = fmap (++ "/.reg/regfile") getHomeDirectory


getRegContents :: IO String
getRegContents = do
    regfile <- getRegfile
    contents <- readFile regfile
    return contents

