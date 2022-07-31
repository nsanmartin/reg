module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified System.IO.Strict as SIO
-- remove this
import System.IO
import qualified Data.Map as Map
import qualified System.Hclip as Hclip
import Data.Char (isSpace)
import Store
import Parse

--
setClipboard :: String -> IO ()
setClipboard = Hclip.setClipboard . rstrip

--

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

data Input = StdInput | RegsInput String | PrintInput Bool

stdInput :: Parser Input
stdInput = pure StdInput


getRegsInput :: Parser Input
getRegsInput = RegsInput
    <$> argument str (metavar "QUERY")

printInput :: Parser Input
printInput = PrintInput
    <$> switch
     ( long "print"
    <> short 'p'
    <> help "Print regs." )

input :: Parser Input
input = getRegsInput <|> stdInput <|> printInput

getManyAt :: [Int] -> [String] -> [String]
getManyAt ix ws = [ws !! i | i <- ix]

reduceRegs :: [Int] -> [String] -> [String]
reduceRegs ixs regLines = map (unwords . (getManyAt ixs) . words)  regLines

go :: Input -> IO ()
-- TODO: implement multiple queries
go (RegsInput query) = do
    contents <- getRegContents
    let (regs, ixs) = (splitRegsStr query)
    let (invalidRegs, regLines) = (splitRegResults $ map (lookupReg (toRegTable contents)) regs)
    let requested = if null ixs
                    then unlines regLines
                    else (unwords $ reduceRegs ixs regLines) ++ "\n"
                         
    let str = requested ++ unlines invalidRegs
    putStr str
    setClipboard str


-- if p is False, -p was not given and then we;re not here
go (PrintInput _) = do
    contents <- getRegContents
    mapM_ putStrLn $ toRegScreen $ lines contents

-- not needed
-- go (PrintInput False) = return ()

go StdInput  = do
    newContents <- SIO.getContents
    regfile <- getRegfile
    oldContents <- SIO.readFile regfile 
    let cappedRegs = toCappedRegs $ joinContents oldContents newContents 
    putStr (unlines $ toRegScreen cappedRegs)
    writeFile regfile (unlines cappedRegs)
    setClipboard newContents


description :: String
description = 
    "This program reads lines from stdin and copy each in a \"register\". " ++
    "It also copies to clipboard the input received.\n" ++ 
    "If no stdin is provided, an arbitrary register, requested by a QUERY, " ++
    "is displayed in stdout and also copied to clipboard.\n" ++ 
    "A query is a string that starts with register identifiers and " ++
    "may optionaly have a `.' (dot) followed by word-indices. The register " ++
    "identifiers are : `" ++ regList ++ "'. So for example: " ++
    "`0ac' refers to the three lines cointained at registers 0, a and c while " ++
    "`017.02' means one line containing the first and third words from registers 0, 1 and 7. " ++
    "That is, when word-indices are used the response is put in one line while when " ++
    "they're absent it would have one line per register in the query."

main :: IO ()
main = go =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc description
     <> header "reg - copy and paste from stdin and clipboard" )

