module Main where
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs)
import System.IO ( FilePath
                 , hPutStrLn
                 , hPutStr
                 , hClose
                 , stderr
                 , openFile
                 , hGetContents
                 , IOMode(ReadMode, WriteMode)
                 )
import System.Directory ( getTemporaryDirectory
                        , removeFile
                        , copyFile
                        )

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

trimLeadWtSpc :: String -> String
trimLeadWtSpc (' ':cs)  = trimLeadWtSpc cs
trimLeadWtSpc ('\t':cs) = trimLeadWtSpc cs
trimLeadWtSpc cs        = cs

writeWith :: (String -> String) -> FilePath -> FilePath -> IO ()
writeWith f inpf outf = do
  tmpfile <- getTemporaryFile
  rHandle <- openFile inpf ReadMode
  wHandle <- openFile tmpfile WriteMode
  input   <- hGetContents rHandle
  hPutStr wHandle (f input)
  hClose rHandle
  hClose wHandle
  copyFile tmpfile outf
  removeFile tmpfile
  putStrLn $ "file \"" ++ outf ++ "\" " ++ operation
    where
      operation
       | inpf == outf = "overwtritten"
       | otherwise    = "written"

getTemporaryFile :: IO FilePath
getTemporaryFile = do
  dir <- getTemporaryDirectory
  return (file dir)
    where
      file dir'
        | needsSlash dir' = dir' ++ "/" ++ filename
        | otherwise       = dir' ++ filename
        where filename = "trimspaces.tmp"

-- because of getTemporaryDirectory:
-- on runhugs (or Debian...?) it does not
-- append a trailing slash,
-- but in runghc (or MacOS...?) it does,
-- and I want this to work
-- on my Pocket CHIP (Debian, runhugs)
-- and on my Mac (MacOS, runghc)
needsSlash :: FilePath -> Bool
needsSlash []   = False
needsSlash path = head (reverse path)  /= '/'

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
