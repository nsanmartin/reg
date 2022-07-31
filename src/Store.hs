module Store
where

import System.Directory
import System.IO
import qualified Data.Map as Map
import qualified System.IO.Strict as SIO
import qualified Data.List as List

--
getRegfile :: IO FilePath
getRegfile = fmap (++ "/.reg/regfile") getHomeDirectory


getRegContents :: IO String
getRegContents = do
    regfile <- getRegfile
    contents <- readFile regfile
    return contents
--

regList :: String
regList = "0123456789abcdefghijklmnopqrstuvwxyz"

getPrefixes :: [String]
getPrefixes =  map (\c -> '"':c:" ") regList

toRegScreen :: [String] -> [String]
toRegScreen contents = map (\(p, r) -> p ++ r) $ zip getPrefixes contents

lookupReg :: (Map.Map Char String) -> Char -> Either Char String
lookupReg table char = case Map.lookup char table of
    Nothing -> Left char
    (Just s) -> Right s

toRegTable :: String -> Map.Map Char String
toRegTable = Map.fromList . (zip regList) . lines 

joinContents :: String -> String -> [String]
joinContents oldContent newContent = 
    (lines newContent) ++ (lines oldContent)

splitRegResults ls = 
    let (l, r) = List.partition (\x -> case x of Left s -> True; _ -> False) ls
    in
        (map showRegFromEither l, map showRegFromEither r)


showRegFromEither :: Either Char String -> String
showRegFromEither (Left c) = "\t`" ++ [c] ++ "' Invalid reg!"
showRegFromEither (Right s) = s

toCappedRegs :: [String] -> [String]
toCappedRegs = take $ length regList

