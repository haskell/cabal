{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones, Simon Marlow 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
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
	rawSystemPath,
	rawSystemExit,
        maybeExit,
	rawSystemPathExit,
        moveSources,
        moduleToFilePath,
        mkLibName,
        currentDir,
        dotToSep,
	withTempFile,
	getOptionsFromSource,
	stripComments,
        defaultPackageDesc,
        findPackageDesc,
	defaultHookedPackageDesc,
	findHookedPackageDesc,
#ifdef DEBUG
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 603 
#include "config.h"
#endif

import Distribution.Compat.RawSystem (rawSystem)
import Distribution.Compat.Exception (finally)
import Distribution.Extension (Extension)
import Distribution.Setup (CompilerFlavor(..))
import Distribution.PreProcess.Unlit (unlit)

import Control.Monad(when)
import Data.Char(isSpace)
import Data.List(nub, isSuffixOf)
import Data.Maybe(mapMaybe)
import System.IO (hPutStr, stderr, hFlush, stdout)
import System.IO.Error
import System.Exit
#if (__GLASGOW_HASKELL__ || __HUGS__) && !defined(mingw32_TARGET_OS)
import System.Posix.Internals (c_getpid)
#endif

import Distribution.Compat.FilePath
	(splitFileName, splitFileExt, joinFileName, joinFileExt,
	pathSeparator)
import System.Directory (getDirectoryContents, getCurrentDirectory,
                        doesFileExist, removeFile)

import Distribution.Compat.Directory (copyFile,findExecutable,createDirectoryIfMissing)

#ifdef DEBUG
import HUnit ((~:), (~=?), Test(..), assertEqual)
#endif

-- -----------------------------------------------------------------------------
-- Utils for setup

die :: String -> IO a
die msg = do hFlush stdout; hPutStr stderr (msg++"\n"); exitWith (ExitFailure 1)

warn :: String -> IO ()
warn msg = do hFlush stdout; hPutStr stderr ("Warning: " ++ msg++"\n")

-- -----------------------------------------------------------------------------
-- rawSystem variants
rawSystemPath :: String -> [String] -> IO ExitCode
rawSystemPath prog args = do
  r <- findExecutable prog
  case r of
    Nothing -> die ("Cannot find: " ++ prog)
    Just path -> rawSystem path args

maybeExit :: IO ExitCode -> IO ()
maybeExit cmd = do
  res <- cmd
  if res /= ExitSuccess
	then exitWith res  
	else return ()

-- Exit with the same exitcode if the subcommand fails
rawSystemExit :: FilePath -> [String] -> IO ()
rawSystemExit path args = do
  putStrLn (path ++ concatMap (' ':) args)
	--ToDo: make command display conditional on -v flag?
  maybeExit $ rawSystem path args

-- Exit with the same exitcode if the subcommand fails
rawSystemPathExit :: String -> [String] -> IO ()
rawSystemPathExit prog args = do
  putStrLn (prog ++ concatMap (' ':) args)
	--ToDo: make command display conditional on -v flag?
  maybeExit $ rawSystemPath prog args


-- ------------------------------------------------------------
-- * File Utilities
-- ------------------------------------------------------------

-- |Get the file path for this particular module.  In the IO monad
-- because it looks for the actual file.  Might eventually interface
-- with preprocessor libraries in order to correctly locate more
-- filenames.
-- Returns Nothing if the file doesn't exist.

moduleToFilePath :: [FilePath] -- ^search locations
                 -> String   -- ^Module Name
                 -> [String] -- ^possible suffixes
                 -> IO [FilePath]

moduleToFilePath pref s possibleSuffixes
    = do let possiblePaths = concatMap (searchModuleToPossiblePaths s possibleSuffixes) pref
         matchList <- mapM (\x -> do y <- doesFileExist x; return (x, y)) possiblePaths
         return [x | (x, True) <- matchList]
    where searchModuleToPossiblePaths :: String -> [String] -> FilePath -> [FilePath]
          searchModuleToPossiblePaths s suffs searchP
              = moduleToPossiblePaths searchP s suffs

-- |Get the possible file paths based on this module name.
moduleToPossiblePaths :: FilePath -- ^search prefix
                      -> String -- ^module name
                      -> [String] -- ^possible suffixes
                      -> [FilePath]
moduleToPossiblePaths searchPref s possibleSuffixes =
  let fname = searchPref `joinFileName` (dotToSep s)
  in [fname `joinFileExt` ext | ext <- possibleSuffixes]

dotToSep :: String -> String
dotToSep = map dts
  where
    dts '.' = pathSeparator
    dts c   = c

-- |Put the source files into the right directory in preperation for
-- something like sdist or installHugs.
moveSources :: FilePath -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^search suffixes
            -> IO ()
moveSources pref targetDir sources searchSuffixes
    = do createDirectoryIfMissing True targetDir
	 -- Create parent directories for everything:
         sourceLocs' <- mapM moduleToFPErr sources
         let sourceLocs = concat sourceLocs'
         let sourceLocsNoPref -- get rid of the prefix, for target location.
                 = if null pref then sourceLocs
                   else map (drop ((length pref) +1)) sourceLocs
	 mapM (createDirectoryIfMissing True)
		  $ nub [fst (splitFileName (targetDir `joinFileName` x))
		   | x <- sourceLocsNoPref, fst (splitFileName x) /= "."]
	 -- Put sources into place:
	 sequence_ [copyFile x (targetDir `joinFileName` y)
                      | (x,y) <- (zip sourceLocs sourceLocsNoPref)]
	 return ()
    where moduleToFPErr m
              = do p <- moduleToFilePath [pref] m searchSuffixes
                   when (null p)
                            (putStrLn ("Error: Could not find module: " ++ m
                                       ++ " with any suffix: " ++ (show searchSuffixes))
                             >> exitWith (ExitFailure 1))
                   return p

-- |The path name that represents the current directory.  May be
-- system-specific.  In Unix, it's "." FIX: What about other arches?
currentDir :: FilePath
currentDir = "."

mkLibName :: FilePath -- ^file Prefix
          -> String   -- ^library name.
          -> String
mkLibName pref lib = pref `joinFileName` ("libHS" ++ lib ++ ".a")

-- -----------------------------------------------------------------------------
-- * temporary file names
-- -----------------------------------------------------------------------------

-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
withTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempFile tmp_dir extn action
  = do x <- getProcessID
       findTempName tmp_dir x
  where 
    findTempName tmp_dir x
      = do let filename = ("tmp" ++ show x) `joinFileExt` extn
	       path = tmp_dir `joinFileName` filename
  	   b  <- doesFileExist path
	   if b then findTempName tmp_dir (x+1)
		else action path `finally` try (removeFile path)

#ifdef mingw32_TARGET_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int
		 -- relies on Int == Int32 on Windows
#elif __GLASGOW_HASKELL__ || __HUGS__
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#else
-- #error ToDo: getProcessID
foreign import ccall unsafe "getpid" getProcessID :: IO Int
#endif

-- ------------------------------------------------------------
-- * options in source files
-- ------------------------------------------------------------

-- |Read the initial part of a source file, before any Haskell code,
-- and return the contents of any OPTIONS or LANGUAGE pragmas.
getOptionsFromSource
    :: FilePath
    -> IO ([Extension],                 -- LANGUAGE pragma, if any
           [(CompilerFlavor,[String])]  -- OPTIONS_FOO pragmas
          )
getOptionsFromSource file = do
    text <- readFile file
    return $ foldr appendOptions ([],[]) $ map getOptions $
	takeWhileJust $ map getPragma $
	filter textLine $ map (dropWhile isSpace) $ lines $
	stripComments True $
	if ".lhs" `isSuffixOf` file then unlit file text else text
  where textLine [] = False
	textLine ('#':_) = False
	textLine _ = True

	getPragma :: String -> Maybe [String]
	getPragma line = case words line of
	    ("{-#" : rest) | last rest == "#-}" -> Just (init rest)
	    _ -> Nothing

	getOptions ("OPTIONS":opts) = ([], [(GHC, opts)])
	getOptions ("OPTIONS_GHC":opts) = ([], [(GHC, opts)])
	getOptions ("OPTIONS_NHC98":opts) = ([], [(NHC, opts)])
	getOptions ("OPTIONS_HUGS":opts) = ([], [(Hugs, opts)])
	getOptions ("LANGUAGE":ws) = (mapMaybe readExtension ws, [])
	  where	readExtension :: String -> Maybe Extension
		readExtension w = case reads w of
		    [(ext, "")] -> Just ext
		    [(ext, ",")] -> Just ext
		    _ -> Nothing
	getOptions _ = ([], [])

	appendOptions (exts, opts) (exts', opts') = (exts++exts', opts++opts')

-- takeWhileJust f = map fromJust . takeWhile isJust
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x:xs) = x : takeWhileJust xs
takeWhileJust _ = []

-- |Strip comments from Haskell source.
stripComments
    :: Bool	-- ^ preserve pragmas?
    -> String	-- ^ input source text
    -> String
stripComments keepPragmas = stripCommentsLevel 0
  where stripCommentsLevel :: Int -> String -> String
	stripCommentsLevel 0 ('-':'-':cs) =	-- FIX: symbols like -->
	    stripCommentsLevel 0 (dropWhile (/= '\n') cs)
	stripCommentsLevel 0 ('{':'-':'#':cs)
	  | keepPragmas = '{' : '-' : '#' : copyPragma cs
	stripCommentsLevel n ('{':'-':cs) = stripCommentsLevel (n+1) cs
	stripCommentsLevel 0 (c:cs) = c : stripCommentsLevel 0 cs
	stripCommentsLevel n ('-':'}':cs) = stripCommentsLevel (n-1) cs
	stripCommentsLevel n (c:cs) = stripCommentsLevel n cs
	stripCommentsLevel _ [] = []

	copyPragma ('#':'-':'}':cs) = '#' : '-' : '}' : stripCommentsLevel 0 cs
	copyPragma (c:cs) = c : copyPragma cs
	copyPragma [] = []




-- ------------------------------------------------------------
-- * Finding the description file
-- ------------------------------------------------------------

oldDescFile = "Setup.description"
cabalExt = "cabal"
buildInfoExt = "buildinfo"

matchesDescFile :: FilePath -> Bool
matchesDescFile p = (snd $ splitFileExt p) == cabalExt
                    || p == oldDescFile

noDesc = die $ "No description file found, please create a cabal-formatted description file with the name <pkgname>." ++ cabalExt

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

-- |Package description file (@<pkgname>.cabal@)
defaultPackageDesc :: IO FilePath
defaultPackageDesc = getCurrentDirectory >>= findPackageDesc

-- |Find a package description file in the given directory.  Looks for
-- .cabal files.
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
#ifdef mingw32_TARGET_OS
       do mp1 <- moduleToFilePath "" "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath "" "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   (Just "Distribution\\Simple\\Build.hs") mp1
          assertEqual "not existing not nothing failed" Nothing mp2,

        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo\\Bar\\Bang.hs","Foo\\Bar\\Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"],
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

        TestCase (do files <- filesWithExtensions "." "description"
                     assertEqual "filesWithExtensions" "Setup.description" (head files))
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
