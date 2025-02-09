module Main where

import Control.Exception
import Control.Monad
import Data.List
import System.FilePath
import Text.Printf

import qualified Data.Map as DM
import qualified System.Directory as S
import qualified System.Environment as S
import qualified System.Exit as S
import qualified System.IO as S
import qualified System.IO.Error as S
import qualified System.Info as S

import qualified System.Console.ANSI as SCA -- from ansi-terminal-0.6

-- VERSION HISTORY:
--   * 1.0.0 < 6/19: original version lists paths
--   * 1.0.1 6/19: added search mode (greatly expanded help, formally added a version string)
vERSION_STRING = "1.0.1"

-------
-- TODO:
--  * normalize the colors in Search Mode.
--    (if any matches in a dir are shadowed, make it bright colored), everything else is dull
--

main = S.getArgs >>= run

fatal :: String -> IO a
fatal s = putStderrLn s >> S.exitFailure
putStderr, putStderrLn :: String -> IO ()
putStderr = hPutColored S.stderr SCA.Vivid SCA.Red
putStderrLn = hPutColoredLn S.stderr SCA.Vivid SCA.Red
hPutColored, hPutColoredLn :: S.Handle -> SCA.ColorIntensity -> SCA.Color -> String -> IO ()
hPutColored h i c s = do
  SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
  S.hPutStr h s
  SCA.hSetSGR h [SCA.Reset]
hPutColoredLn h i c s = do
  SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
  S.hPutStrLn h s
  SCA.hSetSGR h [SCA.Reset]

data Opts = Opts {
    oShowDlls :: Bool
  , oNoExeSuffix :: Bool
  , oMatchSubstring :: Bool
  , oVerbose :: Bool
  , oEnvVar :: String
  , oFiles :: [String]
  } deriving Show

defaultOpts = Opts False False False False "PATH" []

parseOpts :: Int -> [String] -> Opts -> IO Opts
parseOpts _ []     opts = return opts
parseOpts i (a:as) opts
  | a == "-h" || a == "--help" = printUsage >> S.exitSuccess
  | a == "-d" || a == "--dlls" = parseOpts (i+1) as (opts {oShowDlls = True})
  | a == "-m" || a == "--match-substring" = parseOpts (i+1) as (opts {oMatchSubstring = True})
  | a == "-X" || a == "--no-exe-suffix" = parseOpts (i+1) as (opts {oNoExeSuffix = True})
  | a == "-v" || a == "--verbose" = parseOpts (i+1) as (opts {oVerbose = True})
  | a == "-V" || a == "--version" = do putStrLn vERSION_STRING >> S.exitSuccess
  | a == "-e" || a == "--env" =
        if null as then invalidOpt i (a ++ " requires variable name to follow (e.g. -e PATH)")
          else parseOpts (i+2) (tail as) (opts {oEnvVar = head as})
  | "-e=" `isPrefixOf` a = parseOpts (i+1) as (opts {oEnvVar = dropKey a})
  | "--env=" `isPrefixOf` a = parseOpts (i+1) as (opts {oEnvVar = dropKey a})
  | take 1 a == "-" = invalidOpt i $ "invalid option: " ++ a
  | otherwise = parseOpts (i+1) as (opts {oFiles = oFiles opts ++ [a]})
  where dropKey = drop 1 . dropWhile (/='=')

printUsage :: IO ()
printUsage = putStrLn
  $ "usage: hpath [OPTS] <file>*\n" ++
     "where [OPTS]\n" ++
     "  -d    --dlls                 shows DLLs in addition to the executables\n" ++
     "  -e    --env=VAR              overrides the environment variable to search\n" ++
     "  -h    --help                 this message\n" ++
     "  -m    --match-substring=STR  in Search Mode this uses partial substring\n" ++
     "                               matches (e.g. a will match ab, ba, or bab);\n" ++
     "                               without this flag we only take exact matches,\n" ++
     "                               though if -X is not specified we'll also\n" ++
     "                               try common executable suffixes on Windows\n" ++
     "  -X    --no-exe               in Search Mode below will not try and append\n" ++
     "                               a .exe suffix (only on Windows)\n" ++
     "  -V    --version              lists the program version and exits successfully\n" ++
     "                               (immediately)\n" ++
     "  -v    --verbose              increases the output\n" ++
     "\n" ++
     "This program runs in two modes:\n" ++
     " (1) Display Mode:  Without file arguments, it lists an overview of your\n" ++
     "     " ++ path ++ " variable.  Specifically, it shows warnings and errors\n" ++
     "     regarding shadowed DLLs and executables.\n" ++
     "\n" ++
     " (2) Search Mode:  Given one or more <file> arguments, it attempts to locate\n" ++
     "     those in the path, and also warns about name shadowing for that file.\n" ++
     "\n" ++
     "EXAMPLES:" ++
     " > hpath\n" ++
     " ... lists output about your current " ++ path ++ " variable\n" ++
     "\n" ++
     " > hpath ls\n" ++
     " ... searches for ls " ++ if isWindows then "(or ls.exe, ls.bat, etc) " else "" ++ "in " ++ path ++ "\n" ++
     "\n" ++
     " > hpath -m ls\n" ++
     " ... searches for any file where \"ls\" is a substring in " ++ path ++ "\n"
     where path = fmtEnvVarRef (oEnvVar defaultOpts)

invalidOpt :: Int -> String -> IO a
invalidOpt ix s = putStderrLn (locStr ++ s) >> printUsage >> S.exitFailure
  where locStr = if ix >= 0 then "arg "++show ix++": " else ""

-- "VAR" -> "$VAR" or "%VAR%" depending on platform
fmtEnvVarRef :: String -> String
fmtEnvVarRef v = if isWindows then "%" ++ v ++ "%" else "$" ++ v

isWindows :: Bool
isWindows = "mingw" `isInfixOf` S.os

run :: [String] -> IO ()
run args = do
  opts <- parseOpts 1 args defaultOpts
  let env = oEnvVar opts
      exes = oFiles opts
  p <- S.lookupEnv env
  case p of
    Nothing -> fatal $ "can't find " ++ fmtEnvVarRef env ++ " in environment"
    Just val
      | null exes -> analyzePath opts p_tokens
      | otherwise -> searchForExes opts p_tokens exes
      where p_tokens = splitOn isPathSep val

isPathSep :: Char -> Bool
isPathSep = (== searchPathSeparator)

-- *HPath> splitOn (==';') "a;b;c;;d;"
-- ["a","b","c","","d"]
-- *HPath> splitOn (==';') "a;b;c;;d;;"
-- ["a","b","c","","d",""]
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p as = splitNext [] as
  where splitNext bs [] = if null bs then [] else [reverse bs]
        splitNext bs (a:as)
          | p a         = reverse bs : splitNext [] as
          | otherwise   = splitNext (a:bs) as

type ExecMap = DM.Map FilePath FilePath -- (file -> dir)
pmInsert :: ExecMap -> FilePath -> FilePath -> ExecMap
pmInsert pm f d = DM.insert f d pm
pmLookup pm f = DM.lookup f pm

-- (1) Display Mode
analyzePath :: Opts -> [String] -> IO ()
analyzePath opts dirs = do
      (pm,nBad,nDups,nExes) <- foldM accM (DM.empty,0,0,0) dirs
      hPutColoredLn S.stdout SCA.Vivid SCA.White $ "============= SUMMARY ============="
      let badColor = if nBad > 0 then SCA.Red else SCA.White
      hPutColoredLn S.stdout SCA.Vivid badColor  $ printf "  %5d bad path(s)"     nBad
      let shaColor = if nDups > 0 then SCA.Yellow else SCA.White
      hPutColoredLn S.stdout SCA.Vivid shaColor  $ printf "  %5d shadowed exe(s)" nDups
      hPutColoredLn S.stdout SCA.Vivid SCA.White $ printf "  %5d total exe(s)"    nExes
      return ()

  where -- The fold accumulator
        -- If we can't read the directory, print it in red.
        -- Otherwise, hand it and its exe's off to addAndFindShadows
        accM s@(pm,nBad,nDups,nExes) dir = do
          let handler :: SomeException -> IO (ExecMap,Int,Int,Int)
              handler _ = do
                hPutColoredLn S.stdout SCA.Vivid SCA.Red $ dir ++ ": cannot read dir"
                return (pm, nBad + 1, nDups, nExes)
              isExe :: Opts -> FilePath -> Bool
              isExe opts f = any (`isSuffixOf` f) $ dlls ++ ["exe","bat","cmd","com"]
                where dlls = if oShowDlls opts then ["dll"] else []

          (S.getDirectoryContents dir >>=
              (addAndFindShadows s dir . filter (isExe opts))) `catch` handler

        addAndFindShadows :: (ExecMap,Int,Int,Int)
                              -> FilePath
                              -> [FilePath]
                              -> IO (ExecMap,Int,Int,Int)
        addAndFindShadows (pm,nBad,nDups,nExes) dir exes = do
          -- scan files fs in dir, add them to pm if not present, find shadowed
          -- print the dir in normal color if no dups
          --   or yellow if some are shadowed
          -- print the shadowed
          let addFile :: (ExecMap,[(FilePath,FilePath)]) -> FilePath -> (ExecMap,[(FilePath,FilePath)])
              addFile (pm,dups) exe =
                case pmLookup pm exe of
                  Nothing   -> (pmInsert pm exe dir,dups)
                  Just pdir -> (pm,(exe,pdir):dups)
          let (newPm,dups) = foldl addFile (pm,[]) exes

          if null dups then
            hPutColoredLn S.stdout SCA.Vivid SCA.White $ dir -- ++ " (" ++ show (length exes) ++ ")"
            else do
              hPutColoredLn S.stdout SCA.Vivid SCA.Yellow $ dir -- ++ " (" ++ show (length exes) ++ ")"
              forM_ dups $ \(dup,pdir) -> do
                hPutColoredLn S.stdout SCA.Dull SCA.Yellow $ printf "  %-16s shadowed by %s" dup pdir
          return (newPm, nBad, nDups + length dups, nExes + length exes)

-- (2) Search Mode
--
-- Loops through all path components looking for some files by
-- name or substring (with -m)
searchForExes :: Opts -> [String] -> [String] -> IO ()
searchForExes opts dirs searchSet = scanPathComponent fmEmpty dirs
  where scanPathComponent :: FileMap -> [FilePath] -> IO ()
        scanPathComponent fm [] = do
          -- list missing ...
          let missing = searchSet \\ map fmKey fm
          when (not (null missing)) $ do
            hPutColoredLn S.stdout SCA.Vivid SCA.Red $ "COULD NOT LOCATE:"
            forM_ missing $ \m -> do
              hPutColoredLn S.stdout SCA.Vivid SCA.Red $ "  " ++ m
            S.exitFailure
          S.exitSuccess

        scanPathComponent fm (dir:dirs) = do
          let -- paths are all the valid paths in the directory
              scanDir :: [FilePath] -> IO FileMap
              scanDir paths0 = do
                let paths = filter (\f -> f /= "." && f /= "..") paths0

                    -- 'fm' is the current file map
                    -- 'path' is the path within 'dir' that we are checking if it matches
                    accumulateMatching :: FileMap -> FilePath -> IO FileMap
                    accumulateMatching fm path = do
                      let matches :: String -> Bool
                          matches s -- matching strings for this path
                              | oMatchSubstring opts = s `isInfixOf` path
                              | not isWindows || oNoExeSuffix opts = s == path
                              | otherwise = s == path || any (\e -> s ++ e == path) [".exe",".bat",".cmd",".com"]
                      let fmtMatches path strs
                            | length searchSet == 1 = path
                            | otherwise             = path ++ verb_str
                            where verb_str
                                    | oVerbose opts = " (for " ++ show strs ++ ")"
                                    | otherwise = ""
                      case filter matches searchSet of
                        [] -> return fm -- nothing changes
                        ss -> do
                          let shadows = map (flip fmLookup fm) ss
                              color = if null shadows then SCA.Green else SCA.Yellow
                          hPutColoredLn S.stdout SCA.Vivid color $ "  " ++ fmtMatches path ss
                          forM_ shadows $ \shadow -> do
                            case shadow of
                              Just (f,fullFile,defDir)
                               | f == path ->
                                hPutColoredLn S.stdout SCA.Vivid SCA.Yellow $ "    shadowed by " ++ defDir </> fullFile
                              _ -> return ()
                          return $ fm ++ map (\s -> (s,path,dir)) ss

                hPutColoredLn S.stdout SCA.Vivid SCA.White dir
                foldM accumulateMatching fm paths

              -- handleInvalidDir :: FileMap -> SomeException -> IO FileMap
              handleInvalidDir fm e
                | S.isDoesNotExistError e = do
                  hPutColoredLn S.stdout SCA.Vivid SCA.Red $ dir ++ ": non-existent directory"
                  return fm
                | otherwise = throw e

          fm' <- (S.getDirectoryContents dir >>= scanDir) `catch` handleInvalidDir fm
          scanPathComponent fm' dirs

type FileMap = [(String,String,String)]

fmLookup :: String -> FileMap -> Maybe (String,String,String)
fmLookup k fm = find ((==k) . fmKey) fm
-- fmLookupAll :: [String] -> FilePath -> [FilePath]
-- fmLookupAll ks fm = nub
--
fmEmpty = [] :: FileMap

fmKey :: (String,String,String) -> String
fmKey (x,_,_) = x

{-
-- findIn: "fo" ->
-- findIn: "foo" -> ["foo.exe","foo.bat","foo.cmd","foo"]
findIn :: Opts -> String -> [FilePath] -> [FilePath]
findIn opts file paths
  | oMatchSubstring opts = filter (file`isPrefixOf`) paths
  | otherwise = findAllMatching paths
  where findAllMatching [] = []
        findAllMatching (f:fs)
          | fileMatches opts file f = f : findAllMatching fs
          | otherwise               = findAllMatching fs


-- returns all the valid matches of a string against a real file
fileMatches :: Opts -> String -> FilePath -> [FilePath]
fileMatches opts s f
  | not isWindows || oNoExeSuffix opts = tryExtensions [""]
  | otherwise                          = tryExtensions ["",".exe",".bat",".cmd",".com"]
  where tryExtensions (e:es)
          | s == f ++ e = (f ++ e) : tryExtensions es
          | otherwise   = tryExtensions es
        tryExtensions [] = []

-}
