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

fixLines cs = unlines (map trimTrailSpaces $ lines cs)

trimTrailSpaces :: String -> String
trimTrailSpaces cs = reverse (trimLeadSpaces $ reverse cs)

trimLeadSpaces :: String -> String
trimLeadSpaces []       = []
trimLeadSpaces (' ':cs) = trimLeadSpaces cs
trimLeadSpaces cs       = cs
