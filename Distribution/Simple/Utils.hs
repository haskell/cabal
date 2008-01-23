{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -cpp -fffi #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Misc. Utilities, especially file-related utilities.
-- Stuff used by multiple modules that doesn't fit elsewhere.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Utils (
        die,
        dieWithLocation,
        warn, notice, info, debug,
        chattyTry,
        breaks,
	wrapText,
        rawSystemExit,
        rawSystemStdout,
	rawSystemStdout',
        maybeExit,
        xargs,
        matchesDescFile,
	rawSystemPathExit,
        smartCopySources,
        createDirectoryIfMissingVerbose,
        copyFileVerbose,
        copyDirectoryRecursiveVerbose,
        moduleToFilePath,
        moduleToFilePath2,
        mkLibName,
        mkProfLibName,
        mkSharedLibName,
        currentDir,
        dotToSep,
	findFile,
        defaultPackageDesc,
        findPackageDesc,
	defaultHookedPackageDesc,
	findHookedPackageDesc,
        exeExtension,
        objExtension,
        dllExtension,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Control.Monad
    ( when, filterM, unless, liftM2 )
import Data.List
    ( nub, unfoldr )

import System.Directory
    ( getDirectoryContents, getCurrentDirectory, doesDirectoryExist
    , doesFileExist, removeFile )
import System.Environment
    ( getProgName )
import System.Cmd
    ( rawSystem )
import System.Exit
    ( exitWith, ExitCode(..) )
import System.FilePath
    ( takeDirectory, takeExtension, (</>), (<.>), pathSeparator )
import System.Directory
    ( copyFile, findExecutable, createDirectoryIfMissing
    , getTemporaryDirectory )
import System.IO
    ( hPutStrLn, stderr, hFlush, stdout, openFile, IOMode(WriteMode) )
import System.IO.Error
    ( try )
import Control.Exception
    ( bracket )

import Distribution.System
    ( OS(..), os )
import Distribution.Version
    (showVersion)
import Distribution.Package
    (PackageIdentifier(..))

#ifdef __NHC__
import System.Cmd (system)
#else
import Control.Exception (evaluate)
import System.Process (runProcess, waitForProcess)
#endif
import System.IO (hClose)

#if __GLASGOW_HASKELL__ >= 604
import Distribution.Compat.TempFile (openTempFile)
#else
import Distribution.Compat.TempFile (withTempFile)
#endif
import Distribution.Verbosity

#ifdef DEBUG
import Test.HUnit ((~:), (~=?), Test(..), assertEqual)
#endif

-- ------------------------------------------------------------------------------- Utils for setup

dieWithLocation :: FilePath -> (Maybe Int) -> String -> IO a
dieWithLocation fname Nothing msg = die (fname ++ ": " ++ msg)
dieWithLocation fname (Just n) msg = die (fname ++ ":" ++ show n ++ ": " ++ msg)

die :: String -> IO a
die msg = do
  hFlush stdout
  pname <- getProgName
  hPutStrLn stderr (pname ++ ": " ++ msg)
  exitWith (ExitFailure 1)

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: Verbosity -> String -> IO ()
warn verbosity msg = 
  when (verbosity >= normal) $ do
    hFlush stdout
    hPutStrLn stderr ("Warning: " ++ msg)

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
--
notice :: Verbosity -> String -> IO ()
notice verbosity msg =
  when (verbosity >= normal) $
    putStrLn msg

-- | More detail on the operation of some action.
-- 
-- We display these messages when the verbosity level is 'verbose'
--
info :: Verbosity -> String -> IO ()
info verbosity msg =
  when (verbosity >= verbose) $
    putStrLn msg

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: Verbosity -> String -> IO ()
debug verbosity msg =
  when (verbosity >= deafening) $
    putStrLn msg

-- | Perform an IO action, catching any IO exceptions and printing an error
--   if one occurs.
chattyTry :: String  -- ^ a description of the action we were attempting
          -> IO ()   -- ^ the action itself
          -> IO ()
chattyTry desc action =
  catch action $ \exception ->
    putStrLn $ "Error while " ++ desc ++ ": " ++ show exception

-- -----------------------------------------------------------------------------
-- Helper functions

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks _ [] = []
breaks f xs = case span f xs of
                  (_, xs') ->
                      case break f xs' of
                          (v, xs'') ->
                              v : breaks f xs''

-- Wraps a list of words text to a list of lines of a particular width.
wrapText :: Int -> [String] -> [String]
wrapText width = map unwords . wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-- -----------------------------------------------------------------------------
-- rawSystem variants
maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  res <- cmd
  unless (res == ExitSuccess) $ exitWith res

printRawCommandAndArgs :: Verbosity -> FilePath -> [String] -> IO ()
printRawCommandAndArgs verbosity path args
 | verbosity >= deafening = print (path, args)
 | verbosity >= verbose   = putStrLn $ unwords (path : args)
 | otherwise              = return ()

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: Verbosity -> FilePath -> [String] -> IO ()
rawSystemExit verbosity path args = do
  printRawCommandAndArgs verbosity path args
  maybeExit $ rawSystem path args

-- Exit with the same exitcode if the subcommand fails
rawSystemPathExit :: Verbosity -> String -> [String] -> IO ()
rawSystemPathExit verbosity prog args = do
  r <- findExecutable prog
  case r of
    Nothing   -> die ("Cannot find: " ++ prog)
    Just path -> rawSystemExit verbosity path args

-- Run a command and return its output
rawSystemStdout :: Verbosity -> FilePath -> [String] -> IO String
rawSystemStdout verbosity path args = do
  (output, exitCode) <- rawSystemStdout' verbosity path args
  unless (exitCode == ExitSuccess) $ exitWith exitCode
  return output

rawSystemStdout' :: Verbosity -> FilePath -> [String] -> IO (String, ExitCode)
rawSystemStdout' verbosity path args = do
  printRawCommandAndArgs verbosity path args
  tmpDir <- getTemporaryDirectory

#if __GLASGOW_HASKELL__ >= 604
  -- TODO Ideally we'd use runInteractiveProcess and not have to make any
  --      silly temp files, however it is not possible to only connect pipes
  --      to a subset of the process's stdin/out/err. We really cannot
  --      connect to all three since then we'd need threads to pull on stdout
  --      and stderr simultaniously to avoid deadlock, and using threads like
  --      that would not be portable to Hugs for example.
  bracket (liftM2 (,) (openTempFile tmpDir "cmdstdout") (openFile devNull WriteMode))
          -- We need to close tmpHandle or the file removal fails on Windows
          (\((tmpName, tmpHandle), nullHandle) -> do
             hClose tmpHandle
             removeFile tmpName
             hClose nullHandle)
         $ \((tmpName, tmpHandle), nullHandle) -> do
    cmdHandle <- runProcess path args Nothing Nothing
                   Nothing (Just tmpHandle) (Just nullHandle)
    exitCode <- waitForProcess cmdHandle
    output <- readFile tmpName
    evaluate (length output)
    return (output, exitCode)
#else
  withTempFile tmpDir "cmdstdout" $ \tmpName -> do
    let quote name = "'" ++ name ++ "'"
    exitCode <- system $ unwords (map quote (path:args)) ++ " >" ++ quote tmpName
    output <- readFile tmpName
    length output `seq` return (output, exitCode)
#endif

-- | Like the unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- Use it with either of the rawSystem variants above. For example:
-- 
-- > xargs (32*1024) (rawSystemPathExit verbosity) prog fixedArgs bigArgs
--
xargs :: Int -> ([String] -> IO ())
      -> [String] -> [String] -> IO ()
xargs maxSize rawSystemFun fixedArgs bigArgs =
  let fixedArgSize = sum (map length fixedArgs) + length fixedArgs
      chunkSize = maxSize - fixedArgSize
   in mapM_ (rawSystemFun . (fixedArgs ++)) (chunks chunkSize bigArgs)

  where chunks len = unfoldr $ \s ->
          if null s then Nothing
                    else Just (chunk [] len s)

        chunk acc _   []     = (reverse acc,[])
        chunk acc len (s:ss)
          | len' < len = chunk (s:acc) (len-len'-1) ss
          | otherwise  = (reverse acc, s:ss)
          where len' = length s

-- ------------------------------------------------------------
-- * File Utilities
-- ------------------------------------------------------------

-- |Get the file path for this particular module.  In the IO monad
-- because it looks for the actual file.  Might eventually interface
-- with preprocessor libraries in order to correctly locate more
-- filenames.
-- Returns empty list if no such files exist.

moduleToFilePath :: [FilePath] -- ^search locations
                 -> String   -- ^Module Name
                 -> [String] -- ^possible suffixes
                 -> IO [FilePath]

moduleToFilePath pref s possibleSuffixes
    = filterM doesFileExist $
          concatMap (searchModuleToPossiblePaths s possibleSuffixes) pref
    where searchModuleToPossiblePaths :: String -> [String] -> FilePath -> [FilePath]
          searchModuleToPossiblePaths s' suffs searchP
              = moduleToPossiblePaths searchP s' suffs

-- |Like 'moduleToFilePath', but return the location and the rest of
-- the path as separate results.
moduleToFilePath2
    :: [FilePath] -- ^search locations
    -> String   -- ^Module Name
    -> [String] -- ^possible suffixes
    -> IO [(FilePath, FilePath)] -- ^locations and relative names
moduleToFilePath2 locs mname possibleSuffixes
    = filterM exists $
        [(loc, fname <.> ext) | loc <- locs, ext <- possibleSuffixes]
  where
    fname = dotToSep mname
    exists (loc, relname) = doesFileExist (loc </> relname)

-- |Get the possible file paths based on this module name.
moduleToPossiblePaths :: FilePath -- ^search prefix
                      -> String -- ^module name
                      -> [String] -- ^possible suffixes
                      -> [FilePath]
moduleToPossiblePaths searchPref s possibleSuffixes =
  let fname = searchPref </> (dotToSep s)
  in [fname <.> ext | ext <- possibleSuffixes]

findFile :: [FilePath]    -- ^search locations
         -> FilePath      -- ^File Name
         -> IO FilePath
findFile prefPathsIn locPath = do
  let prefPaths = nub prefPathsIn -- ignore dups
  paths <- filterM doesFileExist [prefPath </> locPath | prefPath <- prefPaths]
  case nub paths of -- also ignore dups, though above nub should fix this.
    [path] -> return path
    []     -> die (locPath ++ " doesn't exist")
    paths' -> die (locPath ++ " is found in multiple places:" ++ unlines (map ((++) "    ") paths'))

dotToSep :: String -> String
dotToSep = map dts
  where
    dts '.' = pathSeparator
    dts c   = c

-- |Copy the source files into the right directory.  Looks in the
-- build prefix for files that look like the input modules, based on
-- the input search suffixes.  It copies the files into the target
-- directory.

smartCopySources :: Verbosity -- ^verbosity
            -> [FilePath] -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^search suffixes
            -> Bool     -- ^Exit if no such modules
            -> Bool     -- ^Preserve directory structure
            -> IO ()
smartCopySources verbosity srcDirs targetDir sources searchSuffixes exitIfNone preserveDirs
    = do createDirectoryIfMissingVerbose verbosity True targetDir
         allLocations <- mapM moduleToFPErr sources
         let copies = [(srcDir </> name,
                        if preserveDirs 
                          then targetDir </> srcDir </> name
                          else targetDir </> name) |
                       (srcDir, name) <- concat allLocations]
	 -- Create parent directories for everything:
	 mapM_ (createDirectoryIfMissingVerbose verbosity True) $ nub $
             [takeDirectory targetFile | (_, targetFile) <- copies]
	 -- Put sources into place:
	 sequence_ [copyFileVerbose verbosity srcFile destFile |
                    (srcFile, destFile) <- copies]
    where moduleToFPErr m
              = do p <- moduleToFilePath2 srcDirs m searchSuffixes
                   when (null p && exitIfNone)
                            (die ("Error: Could not find module: " ++ m
                                       ++ " with any suffix: " ++ (show searchSuffixes)))
                   return p

createDirectoryIfMissingVerbose :: Verbosity -> Bool -> FilePath -> IO ()
createDirectoryIfMissingVerbose verbosity parentsToo dir = do
  let msgParents = if parentsToo then " (and its parents)" else ""
  info verbosity ("Creating " ++ dir ++ msgParents)
  createDirectoryIfMissing parentsToo dir

copyFileVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose verbosity src dest = do
  info verbosity ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- adaptation of removeDirectoryRecursive
copyDirectoryRecursiveVerbose :: Verbosity -> FilePath -> FilePath -> IO ()
copyDirectoryRecursiveVerbose verbosity srcDir destDir = do
  info verbosity ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  let aux src dest =
         let cp :: FilePath -> IO ()
             cp f = let srcFile  = src  </> f
                        destFile = dest </> f
                    in  do success <- try (copyFileVerbose verbosity srcFile destFile)
                           case success of
                              Left e  -> do isDir <- doesDirectoryExist srcFile
                                            -- If f is not a directory, re-throw the error
                                            unless isDir $ ioError e
                                            aux srcFile destFile
                              Right _ -> return ()
         in  do createDirectoryIfMissingVerbose verbosity False dest
                getDirectoryContentsWithoutSpecial src >>= mapM_ cp
   in aux srcDir destDir

  where getDirectoryContentsWithoutSpecial =
            fmap (filter (not . flip elem [".", ".."]))
          . getDirectoryContents



-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

mkLibName :: FilePath -- ^file Prefix
          -> String   -- ^library name.
          -> String
mkLibName pref lib = pref </> ("libHS" ++ lib ++ ".a")

mkProfLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkProfLibName pref lib = mkLibName pref (lib++"_p")

-- Implement proper name mangling for dynamical shared objects
-- libHS<packagename>-<compilerFlavour><compilerVersion>
-- e.g. libHSbase-2.1-ghc6.6.1.so
mkSharedLibName :: FilePath        -- ^file Prefix
              -> String            -- ^library name.
              -> PackageIdentifier -- ^package identifier of the compiler
              -> String
mkSharedLibName pref lib (PackageIdentifier compilerName compilerVersion)
  = pref </> ("libHS" ++ lib ++ "-" ++ compiler) <.> dllExtension
  where compiler = compilerName ++ showVersion compilerVersion

-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

oldDescFile :: String
oldDescFile = "Setup.description"

cabalExt :: String
cabalExt = "cabal"

buildInfoExt  :: String
buildInfoExt = "buildinfo"

-- > matchesDescFile "blah.Cabal"
-- > not $ matchesDescFile "blag.bleg"
matchesDescFile :: FilePath -> Bool
matchesDescFile p = (takeExtension p) == '.':cabalExt
                    || p == oldDescFile

noDesc :: IO a
noDesc = die $ "No description file found, please create a cabal-formatted description file with the name <pkgname>." ++ cabalExt

multiDesc :: [String] -> IO a
multiDesc l = die $ "Multiple description files found.  Please use only one of : "
                      ++ show (filter (/= oldDescFile) l)

-- |A list of possibly correct description files.  Should be pre-filtered.
descriptionCheck :: Verbosity -> [FilePath] -> IO FilePath
descriptionCheck _ [] = noDesc
descriptionCheck verbosity [x]
    | x == oldDescFile
        = do warn verbosity $ "The filename \"Setup.description\" is deprecated, please move to <pkgname>." ++ cabalExt
             return x
    | matchesDescFile x = return x
    | otherwise = noDesc
descriptionCheck verbosity [x,y]
    | x == oldDescFile
        = do warn verbosity $ "The filename \"Setup.description\" is deprecated.  Please move out of the way. Using \""
                  ++ y ++ "\""
             return y
    | y == oldDescFile
        = do warn verbosity $ "The filename \"Setup.description\" is deprecated.  Please move out of the way. Using \""
                  ++ x ++ "\""
             return x

    | otherwise = multiDesc [x,y]
descriptionCheck _ l = multiDesc l

-- |Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: Verbosity -> IO FilePath
defaultPackageDesc verbosity
    = getCurrentDirectory >>= findPackageDesc verbosity

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: Verbosity   -- ^Verbosity
                -> FilePath    -- ^Where to look
                -> IO FilePath -- <pkgname>.cabal
findPackageDesc verbosity p
 = do ls <- getDirectoryContents p
      let descs = filter matchesDescFile ls
      descriptionCheck verbosity descs

-- |Optional auxiliary package information file (/pkgname/@.buildinfo@)
defaultHookedPackageDesc :: IO (Maybe FilePath)
defaultHookedPackageDesc = getCurrentDirectory >>= findHookedPackageDesc

-- |Find auxiliary package information in the given directory.
-- Looks for @.buildinfo@ files.
findHookedPackageDesc
    :: FilePath			-- ^Directory to search
    -> IO (Maybe FilePath)	-- ^/dir/@\/@/pkgname/@.buildinfo@, if present
findHookedPackageDesc dir = do
    ns <- getDirectoryContents dir
    case [dir </>  n |
		n <- ns, takeExtension n == '.':buildInfoExt] of
	[] -> return Nothing
	[f] -> return (Just f)
	_ -> die ("Multiple files with extension " ++ buildInfoExt)

-- ------------------------------------------------------------
-- * Platform file extensions
-- ------------------------------------------------------------ 

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
exeExtension = case os of
                   Windows _ -> "exe"
                   _         -> ""

-- ToDo: This should be determined via autoconf (AC_OBJEXT)
-- | Extension for object files. For GHC and NHC the extension is @\"o\"@.
-- Hugs uses either @\"o\"@ or @\"obj\"@ depending on the used C compiler.
objExtension :: String
objExtension = "o"

-- | Extension for dynamically linked (or shared) libraries
-- (typically @\"so\"@ on Unix and @\"dll\"@ on Windows)
dllExtension :: String
dllExtension = case os of
                   Windows _ -> "dll"
                   OSX       -> "dylib"
                   _         -> "so"

devNull :: FilePath
devNull = case os of
                   Windows _ -> "NUL"
                   _         -> "/dev/null"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests
    = let suffixes = ["hs", "lhs"]
          in [TestCase $
       do mp1 <- moduleToFilePath [""] "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath [""] "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   ["Distribution" </> "Simple" </> "Build.hs"] mp1
          assertEqual "not existing not nothing failed" [] mp2,

        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo" </> "Bar" </> "Bang.hs","Foo" </> "Bar" </> "Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"],
        TestCase (do files <- filesWithExtensions "." "cabal"
                     assertEqual "filesWithExtensions" "Cabal.cabal" (head files))
          ]

-- |Might want to make this more generic some day, with regexps
-- or something.
filesWithExtensions :: FilePath -- ^Directory to look in
                    -> String   -- ^The extension
                    -> IO [FilePath] {- ^The file names (not full
                                     path) of all the files with this
                                     extension in this directory. -}
filesWithExtensions dir extension 
    = do allFiles <- getDirectoryContents dir
         return $ filter hasExt allFiles
    where
      hasExt f = takeExtension f == '.':extension
#endif
