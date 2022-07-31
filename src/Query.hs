module Query
where

import qualified Data.Map as Map
import qualified Data.List as List

import Parse

type RegTable = Map.Map Char String

regList :: String
regList = "0123456789abcdefghijklmnopqrstuvwxyz"

getPrefixes :: [String]
getPrefixes =  map (\c -> '"':c:" ") regList

toRegScreen :: [String] -> [String]
toRegScreen contents = map (\(p, r) -> p ++ r) $ zip getPrefixes contents

lookupReg :: RegTable -> Char -> Either Char String
lookupReg table char = case Map.lookup char table of
    Nothing -> Left char -- reg not found
    (Just s) -> Right s

toRegTable :: String -> RegTable
toRegTable = Map.fromList . (zip regList) . lines 


splitRegResults ls = 
    let (l, r) = List.partition (\x -> case x of Left s -> True; _ -> False) ls
    in
        (map showRegFromEither l, map showRegFromEither r)

        where
            showRegFromEither :: Either Char String -> String
            showRegFromEither (Left c) = "\t`" ++ [c] ++ "' Invalid reg!"
            showRegFromEither (Right s) = s


reduceRegs :: [Int] -> [String] -> [String]
reduceRegs ixs regLines = map (unwords . (getManyAt ixs) . words)  regLines
    where
        getManyAt :: [Int] -> [String] -> [String]
        getManyAt ix ws = [ws !! i | i <- ix]

getResponse :: RegTable -> String -> String
getResponse regTable query = 
    let (regs, ixs) = (splitRegsStr query)
        (invalidRegs, regLines) = (splitRegResults $ map (lookupReg regTable) regs)
        requested = if null ixs
                    then regLines
                    else reduceRegs ixs regLines
    in
        unwords requested ++ unlines invalidRegs

