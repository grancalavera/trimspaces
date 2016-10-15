module Main where
import System.Environment (getArgs)
import System.IO ( openFile
                 , hGetContents
                 , hClose
                 , IOMode(ReadMode)
                 )

main = do
  args <- getArgs
  case args of
    [inp,out] -> writeWith fixLines inp out
    [inp]     -> overwriteWith fixLines inp
    _         -> errorMsg "not enough arguments"

writeWith f inpf outf
  | inpf == outf = overwriteWith f inpf
  | otherwise    = do 
                     input <- readFile inpf
                     writeFile outf (f input)
    
overwriteWith f file = do
  hdl  <- openFile file ReadMode
  input <- hGetContents hdl
  hClose hdl
  writeFile file (f input)
  putStrLn $ "file \"" ++ file ++ "\" overwritten" 

fixLines cs = unlines (map trimTrailWtSpc $ lines cs)

trimTrailWtSpc :: String -> String
trimTrailWtSpc cs = reverse (trimLeadWtSpc $ reverse cs)

trimLeadWtSpc :: String -> String
trimLeadWtSpc (' ':cs)  = trimLeadWtSpc cs
trimLeadWtSpc ('\t':cs) = trimLeadWtSpc cs
trimLeadWtSpc cs        = cs

errorMsg msg = do
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
