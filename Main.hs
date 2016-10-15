module Main where
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [inp,out] -> interactWith fixLines inp out
    _         -> putStrLn "error: exactly 2 args required"
    
interactWith f inpf outf = do
  input <- readFile inpf
  writeFile outf (f input)

fixLines cs = unlines (map trimTrailWtSpc $ lines cs)

trimTrailWtSpc :: String -> String
trimTrailWtSpc cs = reverse (trimLeadWtSpc $ reverse cs)

trimLeadWtSpc :: String -> String
trimLeadWtSpc (' ':cs)  = trimLeadWtSpc cs
trimLeadWtSpc ('\t':cs) = trimLeadWtSpc cs
trimLeadWtSpc cs        = cs

