{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Utils
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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
	splitFilenameDir,
	split,
	isPathSeparator,
        pathSeperatorStr,
	setupMessage,
	die,
	findBinary,
	rawSystemPath,
	rawSystemExit,
	rawSystemPathExit,
        moveSources,
        hunitTests,
        createIfNotExists
  ) where

import Distribution.Package (PackageDescription(..), showPackageId)

import Control.Monad(when)
import Data.List(inits, nub, intersperse, findIndices)
import Data.Maybe(Maybe, listToMaybe, isNothing, fromJust)

import System.IO
import System.Exit
import System.Cmd
import System.Environment
import System.Directory

import HUnit ((~:), (~=?), Test(..))

-- -----------------------------------------------------------------------------
-- Pathname-related utils

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitFilenameDir :: String -> (String,String)
splitFilenameDir str
  = let (dir, rest) = split_longest_prefix str isPathSeparator
  	real_dir | null dir  = "."
		 | otherwise = dir
    in  (real_dir, rest)

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest' -> chunk : split c rest'
  where (chunk, rest) = break (==c) s

split_longest_prefix :: String -> (Char -> Bool) -> (String,String)
split_longest_prefix s pred'
  = case pre of
	[]      -> ([], reverse suf)
	(_:pre') -> (reverse pre', reverse suf)
  where (suf,pre) = break pred' (reverse s)

isPathSeparator :: Char -> Bool
isPathSeparator ch =
#ifdef mingw32_TARGET_OS
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif

-- ToDo: add cacheing?
findBinary :: String -> IO (Maybe FilePath)
findBinary binary = do
  path <- getEnv "PATH"
  search (parsePath path)
  where
    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
	let path = d ++ '/':binary
	b <- doesFileExist path
	if b then return (Just path)
             else search ds

parsePath :: String -> [FilePath]
parsePath path = split pathSep path
  where
#ifdef mingw32_TARGET_OS
	pathSep = ';'
#else
	pathSep = ':'
#endif

-- -----------------------------------------------------------------------------
-- Utils for setup

setupMessage :: String -> PackageDescription -> IO ()
setupMessage msg pkg_descr = 
   putStrLn (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")

die :: String -> IO a
die msg = do hPutStr stderr msg; exitWith (ExitFailure 1)

-- -----------------------------------------------------------------------------
-- rawSystem variants

rawSystemPath :: String -> [String] -> IO ExitCode
rawSystemPath prog args = do
  r <- findBinary prog
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
  putStrLn (path ++ concat (map (' ':) args))
	--ToDo: make command display conditional on -v flag?
  maybeExit $ rawSystem path args

-- Exit with the same exitcode if the subcommand fails
rawSystemPathExit :: String -> [String] -> IO ()
rawSystemPathExit prog args = do
  putStrLn (prog ++ concat (map (' ':) args))
	--ToDo: make command display conditional on -v flag?
  maybeExit $ rawSystemPath prog args


-- ------------------------------------------------------------
-- * File Utilities
-- ------------------------------------------------------------


-- |FIX: Do we actually have to make something differnet for windows,
-- or does this work?
pathSeperator :: Char
pathSeperator = '/'

pathSeperatorStr :: String
pathSeperatorStr = [pathSeperator]

createIfNotExists :: Bool     -- ^Create its parents too?
		  -> FilePath -- ^The path to the directory you want to make
		  -> IO ()
createIfNotExists parents file
    = do b <- doesDirectoryExist file
	 case (b,parents, file) of 
		  (_, _, "")    -> return ()
		  (True, _, _)  -> return()
		  (_, True, _)  -> createDirectoryParents file
		  (_, False, _) -> createDirectory file

-- |like mkdir -p.  Create this directory and its parents
createDirectoryParents :: FilePath -> IO()
createDirectoryParents file
    = mapM_ (createIfNotExists False) (pathInits file)

-- |Get this path and all its parents.
pathInits :: FilePath -> [FilePath]
pathInits path
    = map (concat . intersperse pathSeperatorStr)
      (inits $ mySplit pathSeperator path)

-- |Give a list of lists breaking apart elements who match the given criteria

--  > mySplit '.' "foo.bar.bang" => ["foo","bar","bang"] :: [[Char]]
mySplit :: Eq a => a -> [a] -> [[a]]
mySplit a l = let (upto, rest) = break (== a) l
		  in if null rest
		     then [upto]
		     else upto:(mySplit a (tail rest))

-- |Find the last slash and remove it and everything after it. Turns
-- Foo/Bar.lhs into Foo
removeFilename :: FilePath -> FilePath
removeFilename path
    = case findIndices (== pathSeperator) path of
      [] -> path
      l  -> fst $ splitAt (maximum l) path

-- |If this filename doesn't end in the path separator, add it.
maybeAddSep :: FilePath -> FilePath
maybeAddSep [] = []
maybeAddSep p = if last p == pathSeperator then p else p ++ pathSeperatorStr

-- |Get the file path for this particular module.  In the IO monad
-- because it looks for the actual file.  Might eventually interface
-- with preprocessor libraries in order to correctly locate more
-- filenames.
-- Returns Nothing if the file doesn't exist.

moduleToFilePath :: String   -- ^Module Name
                 -> IO (Maybe FilePath)

moduleToFilePath s
    = do let possiblePaths = moduleToPossiblePaths s
         matchList <- sequence $ map (\x -> do y <- doesFileExist x; return (x, y)) possiblePaths
--         sequence $ map (system . ("ls " ++)) possiblePaths
         return $ listToMaybe [x | (x, True) <- matchList]

-- |Get the possible file paths based on this module name.
moduleToPossiblePaths :: String -> [FilePath]
moduleToPossiblePaths s
    =  let splitted = mySplit '.' s
           lastElem = last splitted
           possibleSuffixes = [".hs", ".lhs"]
           pref = if (not $ null $ init splitted)
                  then concat (intersperse pathSeperatorStr (init splitted))
                           ++ pathSeperatorStr
                  else ""
        in [pref ++ x | x <- map (lastElem++) possibleSuffixes]



-- |Put the source files into the right directory in preperation for
-- something like sdist or installHugs.
moveSources :: FilePath   -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^Main modules
            -> IO ()
moveSources _targetDir sources mains
    = do let targetDir = maybeAddSep _targetDir
         createIfNotExists True targetDir
	 -- Create parent directories for everything:
         sourceLocs <- sequence $ map moduleToFPErr (sources ++ mains)
	 mapM (createIfNotExists True)
		  $ nub [(removeFilename $ targetDir ++ x)
		   | x <- sourceLocs, (removeFilename x /= "")]
	 -- Put sources into place:
	 mapM system ["cp -r " ++ x ++ " " ++ targetDir ++ x
				  | x <- sourceLocs]
	 return ()
    where moduleToFPErr m
              = do p <- moduleToFilePath m
                   when (isNothing p)
                            (putStrLn ("Error: Could not find module: " ++ m)
                             >> exitWith (ExitFailure 1))
                   return $ fromJust p

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: IO Test
hunitTests
    = do mp1 <- moduleToFilePath "Distribution.Simple.Build" --exists
         mp2 <- moduleToFilePath "Foo.Bar"      -- doesn't exist
         return $ TestLabel "Utils Tests" $ TestList
             ["moduleToPossiblePaths 1" ~: "failed" ~:
              ["Foo/Bar/Bang.hs","Foo/Bar/Bang.lhs"]
                ~=? (moduleToPossiblePaths "Foo.Bar.Bang"),
              "moduleToPossiblePaths2 " ~: "failed" ~:
                (moduleToPossiblePaths "Foo")
                ~=? ["Foo.hs", "Foo.lhs"],


              "existing not found" ~: "failed" ~:
                   (Just "Distribution/Simple/Build.hs") ~=? mp1,
              "not existing not nothing" ~: "failed" ~: Nothing ~=? mp2
             ]
#endif
