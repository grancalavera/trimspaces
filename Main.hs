module Main where
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main = do
  args <- getArgs
  case args of
    []        -> putStrLn usage
    [inp]     -> overwriteWith fixLines inp
    [inp,out] -> writeWith fixLines inp out
    _         -> die "error: too many arguments"

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

usage = unlines
  [ ""
  , "usage:"
  , ""
  , "trimspaces <input>           trim trailing"
  , "                             whitespace from"
  , "                             <input> and"
  , "                             overwrite result"
  , "                             back to <input>"
  , ""
  , "trimspaces <input> <output>  trim trailing"
  , "                             whitespace from"
  , "                             <input> and write"
  , "                             result to <output>"
  , ""
  ]

die msg = do
  putStrLn ""
  hPutStrLn stderr msg
  putStrLn usage
  exitWith (ExitFailure 1)
