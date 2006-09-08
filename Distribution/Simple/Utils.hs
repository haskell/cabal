{-# OPTIONS_GHC -cpp -fffi #-}
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
	warn,
	rawSystemPath,
        rawSystemVerbose,
	rawSystemExit,
        maybeExit,
        xargs,
        matchesDescFile,
	rawSystemPathExit,
        smartCopySources,
        copyFileVerbose,
        copyDirectoryRecursiveVerbose,
        moduleToFilePath,
        mkLibName,
        mkProfLibName,
        currentDir,
	dirOf,
        dotToSep,
	withTempFile,
	findFile,
        defaultPackageDesc,
        findPackageDesc,
	defaultHookedPackageDesc,
	findHookedPackageDesc,
        distPref,
        haddockPref,
        srcPref,
#ifdef DEBUG
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Compat.RawSystem (rawSystem)
import Distribution.Compat.Exception (finally)

import Control.Monad(when, filterM, unless)
import Data.List (nub, unfoldr)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.IO.Error
import System.Exit
#if (__GLASGOW_HASKELL__ || __HUGS__) && !(mingw32_HOST_OS || mingw32_TARGET_OS)
import System.Posix.Internals (c_getpid)
#endif

import Distribution.Compat.FilePath
	(splitFileName, splitFileExt, joinFileName, joinFileExt, joinPaths,
	pathSeparator,splitFilePath)
import System.Directory (getDirectoryContents, getCurrentDirectory
			, doesDirectoryExist, doesFileExist, removeFile, getPermissions
			, Permissions(executable))

import Distribution.Compat.Directory
           (copyFile, findExecutable, createDirectoryIfMissing,
            getDirectoryContentsWithoutSpecial)

#ifdef DEBUG
import HUnit ((~:), (~=?), Test(..), assertEqual)
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

warn :: String -> IO ()
warn msg = do
  hFlush stdout
  pname <- getProgName
  hPutStrLn stderr (pname ++ ": Warning: " ++ msg)

-- -----------------------------------------------------------------------------
-- rawSystem variants
rawSystemPath :: Int -> String -> [String] -> IO ExitCode
rawSystemPath verbose prog args = do
  r <- findExecutable prog
  case r of
    Nothing   -> die ("Cannot find: " ++ prog)
    Just path -> rawSystemVerbose verbose path args

rawSystemVerbose :: Int -> FilePath -> [String] -> IO ExitCode
rawSystemVerbose verbose prog args = do
      when (verbose > 0) $
        putStrLn (prog ++ concatMap (' ':) args)
      e <- doesFileExist prog
      if e
         then do perms <- getPermissions prog
                 if (executable perms)
                    then rawSystem prog args
                    else die ("Error: file is not executable: " ++ show prog)
         else die ("Error: file does not exist: " ++ show prog)

maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  res <- cmd
  if res /= ExitSuccess
	then exitWith res  
	else return ()

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: Int -> FilePath -> [String] -> IO ()
rawSystemExit verbose path args = do
  when (verbose > 0) $
    putStrLn (path ++ concatMap (' ':) args)
  maybeExit $ rawSystem path args

-- Exit with the same exitcode if the subcommand fails
rawSystemPathExit :: Int -> String -> [String] -> IO ()
rawSystemPathExit verbose prog args = do
  maybeExit $ rawSystemPath verbose prog args

-- | Like the unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- Use it with either of the rawSystem variants above. For example:
-- 
-- > xargs (32*1024) (rawSystemPath verbose) prog fixedArgs bigArgs
--
xargs :: Int -> (FilePath -> [String] -> IO ExitCode)
      -> FilePath -> [String] -> [String] -> IO ExitCode
xargs maxSize rawSystem prog fixedArgs bigArgs =
  let fixedArgSize = sum (map length fixedArgs) + length fixedArgs
      chunkSize = maxSize - fixedArgSize
      loop [] = return ExitSuccess
      loop (args:remainingArgs) = do
        status <- rawSystem prog (fixedArgs ++ args)
        case status of
          ExitSuccess -> loop remainingArgs
          _           -> return status
   in loop (chunks chunkSize bigArgs)

  where chunks len = unfoldr $ \s ->
          if null s then Nothing
                    else Just (chunk [] len s)

        chunk acc len []     = (reverse acc,[])
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
        [(loc, fname `joinFileExt` ext) | loc <- locs, ext <- possibleSuffixes]
  where
    fname = dotToSep mname
    exists (loc, relname) = doesFileExist (loc `joinFileName` relname)

-- |Get the possible file paths based on this module name.
moduleToPossiblePaths :: FilePath -- ^search prefix
                      -> String -- ^module name
                      -> [String] -- ^possible suffixes
                      -> [FilePath]
moduleToPossiblePaths searchPref s possibleSuffixes =
  let fname = searchPref `joinFileName` (dotToSep s)
  in [fname `joinFileExt` ext | ext <- possibleSuffixes]

findFile :: [FilePath]    -- ^search locations
         -> FilePath      -- ^File Name
         -> IO FilePath
findFile prefPathsIn locPath = do
  let prefPaths = nub prefPathsIn -- ignore dups
  paths <- filterM doesFileExist [prefPath `joinFileName` locPath | prefPath <- prefPaths]
  case nub paths of -- also ignore dups, though above nub should fix this.
    [path] -> return path
    []     -> die (locPath ++ " doesn't exist")
    paths  -> die (locPath ++ " is found in multiple places:" ++ unlines (map ((++) "    ") paths))

dotToSep :: String -> String
dotToSep = map dts
  where
    dts '.' = pathSeparator
    dts c   = c

-- |Copy the source files into the right directory.  Looks in the
-- build prefix for files that look like the input modules, based on
-- the input search suffixes.  It copies the files into the target
-- directory.

smartCopySources :: Int      -- ^verbose
            -> [FilePath] -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^search suffixes
            -> Bool     -- ^Exit if no such modules
            -> Bool     -- ^Preserve directory structure
            -> IO ()
smartCopySources verbose srcDirs targetDir sources searchSuffixes exitIfNone preserveDirs
    = do createDirectoryIfMissing True targetDir
         allLocations <- mapM moduleToFPErr sources
         let copies = [(srcDir `joinFileName` name,
                        if preserveDirs 
                          then targetDir `joinFileName` srcDir `joinFileName` name
                          else targetDir `joinFileName` name) |
                       (srcDir, name) <- concat allLocations]
	 -- Create parent directories for everything:
	 mapM_ (createDirectoryIfMissing True) $ nub $
             [fst (splitFileName targetFile) | (_, targetFile) <- copies]
	 -- Put sources into place:
	 sequence_ [copyFileVerbose verbose srcFile destFile |
                    (srcFile, destFile) <- copies]
    where moduleToFPErr m
              = do p <- moduleToFilePath2 srcDirs m searchSuffixes
                   when (null p && exitIfNone)
                            (die ("Error: Could not find module: " ++ m
                                       ++ " with any suffix: " ++ (show searchSuffixes)))
                   return p

copyFileVerbose :: Int -> FilePath -> FilePath -> IO ()
copyFileVerbose verbose src dest = do
  when (verbose > 0) $
    putStrLn ("copy " ++ src ++ " to " ++ dest)
  copyFile src dest

-- adaptation of removeDirectoryRecursive
copyDirectoryRecursiveVerbose :: Int -> FilePath -> FilePath -> IO ()
copyDirectoryRecursiveVerbose verbose srcDir destDir = do
  when (verbose > 0) $
    putStrLn ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  let aux src dest =
         let cp :: FilePath -> IO ()
             cp f = let srcFile  = joinPaths src  f
                        destFile = joinPaths dest f
                    in  do success <- try (copyFileVerbose verbose srcFile destFile)
                           case success of
                              Left e  -> do isDir <- doesDirectoryExist srcFile
                                            -- If f is not a directory, re-throw the error
                                            unless isDir $ ioError e
                                            aux srcFile destFile
                              Right _ -> return ()
         in  do createDirectoryIfMissing False dest
                getDirectoryContentsWithoutSpecial src >>= mapM_ cp
   in aux srcDir destDir




-- | The path name that represents the current directory.
-- In Unix, it's @\".\"@, but this is system-specific.
-- (E.g. AmigaOS uses the empty string @\"\"@ for the current directory.)
currentDir :: FilePath
currentDir = "."

dirOf :: FilePath -> FilePath
dirOf f = (\ (x, _, _) -> x) $ (splitFilePath f)

mkLibName :: FilePath -- ^file Prefix
          -> String   -- ^library name.
          -> String
mkLibName pref lib = pref `joinFileName` ("libHS" ++ lib ++ ".a")

mkProfLibName :: FilePath -- ^file Prefix
              -> String   -- ^library name.
              -> String
mkProfLibName pref lib = mkLibName pref (lib++"_p")

-- ------------------------------------------------------------
-- * Some Paths
-- ------------------------------------------------------------
distPref :: FilePath
distPref = "dist"

srcPref :: FilePath
srcPref = distPref `joinFileName` "src"

haddockPref :: FilePath
haddockPref = foldl1 joinPaths [distPref, "doc", "html"]

-- ------------------------------------------------------------
-- * temporary file names
-- ------------------------------------------------------------

-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
withTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempFile tmp_dir extn action
  = do x <- getProcessID
       findTempName x
  where 
    findTempName x
      = do let filename = ("tmp" ++ show x) `joinFileExt` extn
	       path = tmp_dir `joinFileName` filename
  	   b  <- doesFileExist path
	   if b then findTempName (x+1)
		else action path `finally` try (removeFile path)

#if mingw32_HOST_OS || mingw32_TARGET_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int
		 -- relies on Int == Int32 on Windows
#elif __GLASGOW_HASKELL__ || __HUGS__
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#else
-- error ToDo: getProcessID
foreign import ccall unsafe "getpid" getProcessID :: IO Int
#endif

-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

oldDescFile :: String
oldDescFile = "Setup.description"

cabalExt :: String
cabalExt = "cabal"

buildInfoExt  :: String
buildInfoExt = "buildinfo"

matchesDescFile :: FilePath -> Bool
matchesDescFile p = (snd $ splitFileExt p) == cabalExt
                    || p == oldDescFile

noDesc :: IO a
noDesc = die $ "No description file found, please create a cabal-formatted description file with the name <pkgname>." ++ cabalExt

multiDesc :: [String] -> IO a
multiDesc l = die $ "Multiple description files found.  Please use only one of : "
                      ++ show (filter (/= oldDescFile) l)

-- |A list of possibly correct description files.  Should be pre-filtered.
descriptionCheck :: [FilePath] -> IO FilePath
descriptionCheck [] = noDesc
descriptionCheck [x]
    | x == oldDescFile
        = do warn $ "The filename \"Setup.description\" is deprecated, please move to <pkgname>." ++ cabalExt
             return x
    | matchesDescFile x = return x
    | otherwise = noDesc
descriptionCheck [x,y]
    | x == oldDescFile
        = do warn $ "The filename \"Setup.description\" is deprecated.  Please move out of the way. Using \""
                  ++ y ++ "\""
             return y
    | y == oldDescFile
        = do warn $ "The filename \"Setup.description\" is deprecated.  Please move out of the way. Using \""
                  ++ x ++ "\""
             return x

    | otherwise = multiDesc [x,y]
descriptionCheck l = multiDesc l

-- |Package description file (/pkgname/@.cabal@)
defaultPackageDesc :: IO FilePath
defaultPackageDesc = getCurrentDirectory >>= findPackageDesc

-- |Find a package description file in the given directory.  Looks for
-- @.cabal@ files.
findPackageDesc :: FilePath    -- ^Where to look
                -> IO FilePath -- <pkgname>.cabal
findPackageDesc p = do ls <- getDirectoryContents p
                       let descs = filter matchesDescFile ls
                       descriptionCheck descs

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
    case [dir `joinFileName`  n |
		n <- ns, snd (splitFileExt n) == buildInfoExt] of
	[] -> return Nothing
	[f] -> return (Just f)
	_ -> die ("Multiple files with extension " ++ buildInfoExt)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests
    = let suffixes = ["hs", "lhs"]
          in [TestCase $
#if mingw32_HOST_OS || mingw32_TARGET_OS
       do mp1 <- moduleToFilePath [""] "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath [""] "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   ["Distribution\\Simple\\Build.hs"] mp1
          assertEqual "not existing not nothing failed" [] mp2,

        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo\\Bar\\Bang.hs","Foo\\Bar\\Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"],
        TestCase (do files <- filesWithExtensions "." "cabal"
                     assertEqual "filesWithExtensions" "Cabal.cabal" (head files))
#else
       do mp1 <- moduleToFilePath [""] "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath [""] "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   ["Distribution/Simple/Build.hs"] mp1
          assertEqual "not existing not nothing failed" [] mp2,

        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo/Bar/Bang.hs","Foo/Bar/Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"],

        TestCase (do files <- filesWithExtensions "." "cabal"
                     assertEqual "filesWithExtensions" "Cabal.cabal" (head files))
#endif
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
      hasExt f = snd (splitFileExt f) == extension
#endif
