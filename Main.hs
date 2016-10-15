module Main where
import System.Environment (getArgs)
import System.IO ( openFile
                 , hGetContents
                 , hClose
                 , hPutStr
                 , IOMode(ReadMode, WriteMode)
                 )

main = do
  args <- getArgs
  case args of
    [inp,out] -> writeWith fixLines inp out
    [inp]     -> writeWith fixLines inp inp
    _         -> usage "not enough arguments"

writeWith f inpf outf = do
  input <- readFile inpf
  -- because of lazyness, thus forcing stricness
  -- http://stackoverflow.com/a/2530948/824779
  id input `seq` (writeFile outf $ f input)

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

