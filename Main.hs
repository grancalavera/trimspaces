module Main where
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [inp]     -> overwriteWith fixLines inp
    [inp,out] -> writeWith fixLines inp out
    _         -> usage "not enough arguments"

writeWith f inpf outf = do
  input <- readFile inpf
  input `seq` (writeFile outf $ f input)

overwriteWith f inpf = do
  writeWith f inpf inpf
  putStrLn $ "file \"" ++ inpf ++ "\" ovewritten"

fixLines cs = unlines (map trimTrailWtSpc $ lines cs)

trimTrailWtSpc :: String -> String
trimTrailWtSpc cs = reverse (trimLeadWtSpc $ reverse cs)

trimLeadWtSpc :: String -> String
trimLeadWtSpc (' ':cs)  = trimLeadWtSpc cs
trimLeadWtSpc ('\t':cs) = trimLeadWtSpc cs
trimLeadWtSpc cs        = cs

usage msg = do
  putStrLn $ msg
    ++  "\nusage:"
    ++  "\ntrimspaces <input> <output>  trim trailing"
    ++  "\n                             whitespace from"
    ++  "\n                             <input> and write"
    ++  "\n                             result to <output>"
    ++  "\ntrimspaces <input>           trim trailing"
    ++  "\n                             whitespace from"
    ++  "\n                             <input> and" 
    ++  "\n                             overwrite result"
    ++  "\n                             back to <input>"

