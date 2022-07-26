module Store
where

import System.Directory
import System.IO
import qualified Data.Map as Map

regs :: String
regs = "0123456789abcdefghijklmnopqrstuvwxyz*+"

getPrefixes :: [String]
getPrefixes =  map (\c -> '"':c:" ") regs

getRegfileScreen :: IO [String]
getRegfileScreen = do
    regfile <- getRegfile
    contents <- readFile regfile
    return $
        map (\(p, r) -> p ++ r) $ zip getPrefixes (lines contents)

getRegfile :: IO FilePath
getRegfile = fmap (++ "/.reg/regfile") getHomeDirectory


getRegContents :: IO String
getRegContents = do
    regfile <- getRegfile
    contents <- readFile regfile
    return contents


getRegTable :: IO (Map.Map Char String)
getRegTable = do
    regfile <- getRegfile
    contents <- readFile regfile
    return  (Map.fromList $ zip regs (lines contents))

filterNothing :: String ->  [Maybe String] -> [String]
filterNothing str = foldr (\a b-> case a of Nothing -> (str:b) ; Just x -> (x:b)) []
-- storeRegs :: [String]  -> IO ()
-- storeRegs ls = do 

