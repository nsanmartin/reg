module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data Input = StdInput { print :: Bool } | GetRegsInput [String]

stdInput :: Parser Input
stdInput = StdInput
         <$> switch
          ( long "print"
         <> short 'p'
         <> help "Print regs (vs print stdin) after registering the input." )


getRegsInput :: Parser Input
getRegsInput = GetRegsInput <$> some (argument str (metavar "REGISTERS..."))

input :: Parser Input
input = getRegsInput <|> stdInput

getReg :: Char -> String
getReg c = ['\'', c, '\'']

getRegBlock :: String -> String
getRegBlock regs = concat $ map getReg regs 

go :: Input -> IO ()
go (GetRegsInput regs) = putStrLn "regs:\n" >> mapM_ (putStrLn . getRegBlock) regs
go (StdInput False) = getContents >>= putStrLn
go (StdInput True) = getContents >> putStrLn "print regs"

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

