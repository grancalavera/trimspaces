module Main where
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs)
import System.IO (FilePath, hPutStrLn, stderr)

main:: IO ()
main = do
  args <- getArgs
  case args of
    []        -> putStrLn usage
    [inp]     -> writeWith trimspaces inp inp
    [inp,out] -> writeWith trimspaces inp out
    _         -> die "error: too many arguments"

trimspaces :: String -> String
trimspaces cs = unlines (map trimTrailWtSpc $ lines cs)

trimTrailWtSpc :: String -> String
trimTrailWtSpc cs = reverse (trimLeadWtSpc $ reverse cs)

trimLeadWtSpc (' ':cs)  = trimLeadWtSpc cs
trimLeadWtSpc ('\t':cs) = trimLeadWtSpc cs
trimLeadWtSpc cs        = cs

writeWith :: (String -> String) -> FilePath -> FilePath -> IO ()
writeWith f inpf outf = do
  input <- readFile inpf
  input `seq` (writeFile outf $ f input)
  putStrLn $ "file \"" ++ outf ++ "\" " ++ operation
    where
      operation
       | inpf == outf = "overwtritten"
       | otherwise    = "written"

usage :: String
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

die :: String -> IO ()
die msg = do
  putStrLn ""
  hPutStrLn stderr msg
  putStrLn usage
  exitWith (ExitFailure 1)
