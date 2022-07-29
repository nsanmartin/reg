module Store
where

import System.Directory
import System.IO
import qualified Data.Map as Map
import qualified System.IO.Strict as SIO
import qualified Data.List as List

regs :: String
regs = "0123456789abcdefghijklmnopqrstuvwxyz*+"

getPrefixes :: [String]
getPrefixes =  map (\c -> '"':c:" ") regs

toRegScreen :: [String] -> [String]
toRegScreen contents = map (\(p, r) -> p ++ r) $ zip getPrefixes contents

getRegfile :: IO FilePath
getRegfile = fmap (++ "/.reg/regfile") getHomeDirectory


getRegContents :: IO String
getRegContents = do
    regfile <- getRegfile
    contents <- readFile regfile
    return contents

lookupReg :: (Map.Map Char String) -> Char -> Either String Char
lookupReg table char = case Map.lookup char table of
    Nothing -> Right char
    (Just s) -> Left s

toRegTable :: String -> Map.Map Char String
toRegTable = Map.fromList . (zip regs) . lines 

joinContents :: String -> String -> [String]
joinContents oldContent newContent = 
    (lines newContent) ++ (lines oldContent)

splitRegResults :: [Either String Char] -> ([Either String Char], [Either String Char])
splitRegResults ls = List.partition (\x -> case x of Left s -> True; _ -> False) ls

showInvalidReg :: Either String Char -> String
showInvalidReg (Left s) = s
showInvalidReg (Right c) = "\t`" ++ [c] ++ "' Invalid reg!"

storeRegs :: String -> [String] -> IO ()
storeRegs regfile joined =
    let len = length regs
        lenJoined = take len joined
        in do
            writeFile regfile (unlines lenJoined)
            putStrLn $ unlines $ toRegScreen lenJoined
    

