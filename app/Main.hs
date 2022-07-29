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

setClipboard = Hclip.setClipboard . rstrip

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

data Input = StdInput | RegsInput String | PrintInput Bool

stdInput :: Parser Input
stdInput = pure StdInput


getRegsInput :: Parser Input
getRegsInput = RegsInput
    <$> argument str (metavar "REGISTERS...")

printInput :: Parser Input
printInput = PrintInput
    <$> switch
     ( long "print"
    <> short 'p'
    <> help "Print regs (vs print stdin) after registering the input." )

input :: Parser Input
input = getRegsInput <|> stdInput <|> printInput

go :: Input -> IO ()
go (RegsInput regs) = do
    contents <- getRegContents
    let (left, right) = splitRegResults $ map (lookupReg (toRegTable contents)) regs in
        putStrLn $ unlines $ map showInvalidReg left ++ (map showInvalidReg right)

-- if p is False, -p was not given and then we;re not here
go (PrintInput _) = do
    contents <- getRegContents
    --regScreen <- getRegfileScreen
    mapM_ putStrLn $ toRegScreen $ lines contents

-- not needed
-- go (PrintInput False) = return ()

go StdInput  = do
    newContents <- SIO.getContents
    regfile <- getRegfile
    oldContents <- SIO.readFile regfile 
    storeRegs regfile $ joinContents oldContents newContents 
    setClipboard newContents
    --putStrLn newContents


description :: String
description = 
    "This program reads lines from stdin and copy each in a vim like \"register\". " ++
    "The last line read is also copied to cipboard.\n" ++ 
    "If no stdin is provided, an arbitrary register can by displayed in stdout " ++
    "and also copied to clipboard."

main :: IO ()
main = go =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc description
     <> header "reg - copy and paste from stdin and clipboard" )

