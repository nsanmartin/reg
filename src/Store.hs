module Store
where

import System.Directory
import System.IO
import qualified Data.Map as Map
import qualified System.IO.Strict as SIO

regs :: String
regs = "0123456789abcdefghijklmnopqrstuvwxyz*+"

getPrefixes :: [String]
getPrefixes =  map (\c -> '"':c:" ") regs

toRegScreen :: [String] -> [String]
toRegScreen contents = map (\(p, r) -> p ++ r) $ zip getPrefixes contents

getRegfileScreen :: IO [String]
getRegfileScreen = do
    regfile <- getRegfile
    contents <- readFile regfile
    return $ toRegScreen $ lines contents

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

joinContents :: String -> String -> [String]
joinContents oldContent newContent = 
    (lines newContent) ++ (lines oldContent)


filterNothing :: String ->  [Maybe String] -> [String]
filterNothing str = foldr (\a b-> case a of Nothing -> (str:b) ; Just x -> (x:b)) []

storeRegs :: String -> [String] -> IO ()
storeRegs regfile joined =
    let len = length regs
        lenJoined = take len joined
        in do
            writeFile regfile (unlines lenJoined)
            putStrLn $ unlines $ toRegScreen lenJoined
    

