module Main where

-- import Lib
-- 
-- main :: IO ()
-- main = someFunc
import Options.Applicative
import Data.Semigroup ((<>))

data Input = StdInput | PrintInput Bool | GetRegsInput [String]

stdInput :: Parser Input
stdInput = pure StdInput

printInput :: Parser Input
printInput = PrintInput
         <$> switch
          ( long "print"
         <> short 'p'
         <> help "Print regs" )


getRegsInput :: Parser Input
getRegsInput = GetRegsInput <$> some (argument str (metavar "SOMEARGS..."))

input :: Parser Input
input = printInput <|> getRegsInput <|> stdInput


greet :: Input -> IO ()
greet (PrintInput True) = putStrLn "p"
greet (GetRegsInput xss@(x:_)) = putStrLn "inputs"
greet StdInput = putStrLn "std input"
greet _ = putStrLn "default"


main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

