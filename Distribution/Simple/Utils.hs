{-# OPTIONS -cpp #-}
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
	splitFilenameDir,
	joinFilenameDir,
	split,
	isPathSeparator,
        pathSeparatorStr,
	setupMessage,
	die,
	findBinary,
	rawSystemPath,
	rawSystemExit,
        maybeExit,
	rawSystemPathExit,
        moveSources,
        moduleToFilePath,
        createIfNotExists,
        mkLibName,
        copyFile,
        pathJoin,
        removeFileRecursive,
        splitExt,
        joinExt,
#ifdef DEBUG
        hunitTests
#endif
  ) where

import Distribution.Package (PackageDescription(..), showPackageId)

import Control.Monad(when, unless, liftM, mapM)
import Data.List(inits, nub, intersperse, findIndices, partition)
import Data.Maybe(Maybe, listToMaybe, isNothing, fromJust, catMaybes)
import System.IO (hPutStr, stderr
#ifdef __GLASGOW_HASKELL__
                 , openBinaryFile, IOMode(..), hGetBuf, hPutBuf, hClose
#endif
                 )
import System.IO.Error
import System.Exit
import Compat.RawSystem (rawSystem)
import Compat.Exception (bracket)
import System.Environment
import System.Directory
import Foreign.Marshal (allocaBytes)
#ifdef HAVE_UNIX_PACKAGE
import System.Posix.Files (getFileStatus, accessTime, modificationTime, setFileTimes)
#endif

#ifdef DEBUG
import HUnit ((~:), (~=?), Test(..), assertEqual)
#endif

-- -----------------------------------------------------------------------------
-- Pathname-related utils

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitFilenameDir :: String -> (String,String)
splitFilenameDir str
  = let (dir, rest) = split_longest_prefix str isPathSeparator
  	real_dir | null dir  = "."
		 | otherwise = dir
    in  (real_dir, rest)

-- "foo/bar" "xyzzy.ext" -> "foo/bar/xyzzy.ext"
joinFilenameDir :: String -> String -> FilePath
joinFilenameDir dir fname = dir++pathSeparator:fname

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

-- | Split the path into filename and extension
splitExt :: FilePath -> (String, String)
splitExt p
  = case pre of
	[]       -> (reverse suf, [])
	(_:pre') -> (reverse pre', reverse suf)
  where (suf,pre) = break (== '.') (reverse p)

-- |Split the path into filename and extension
joinExt :: FilePath -> String -> String
joinExt path ""  = path
joinExt path ext = path ++ '.':ext

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
	let path = d `joinFilenameDir` binary `joinExt` exeSuffix
	b <- doesFileExist path
	if b then return (Just path)
             else search ds

exeSuffix :: String
#ifdef mingw32_TARGET_OS
exeSuffix = "exe"
#else
exeSuffix = ""
#endif

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
die msg = do hPutStr stderr (msg++"\n"); exitWith (ExitFailure 1)

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


-- |FIX: Do we actually have to make something different for windows,
-- or does this work?
pathSeparator :: Char
#ifdef mingw32_TARGET_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

pathSeparatorStr :: String
pathSeparatorStr = [pathSeparator]

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
    = map (\x -> device++pathJoin x) (tail $ inits $ mySplit pathSeparator path')
    where
#ifdef mingw32_TARGET_OS    -- Should we do this only on Windows? What about file://usr/bin?
       (device,path') = case break (== ':') path of
         (path,"") -> ("",path)
         (device,c1:c2:path) | isPathSeparator c2 -> (device++[c1,c2],path)
         (device,c1:path) -> (device++[c1],path)
#else
       (device,path') = ("",path)
#endif

-- |Give a list of lists breaking apart elements who match the given criteria

--  > mySplit '.' "foo.bar.bang" => ["foo","bar","bang"] :: [[Char]]
mySplit :: Eq a => a -> [a] -> [[a]]
mySplit a l = let (upto, rest) = break (== a) l
		  in if null rest
		     then [upto]
		     else upto:(mySplit a (tail rest))

-- |Find the last slash and remove it and everything after it. Turns
-- Foo\/Bar.lhs into Foo
removeFilename :: FilePath -> FilePath
removeFilename path
    = case findIndices (== pathSeparator) path of
      [] -> path
      l  -> fst $ splitAt (maximum l) path

-- |If this filename doesn't end in the path separator, add it.
maybeAddSep :: FilePath -> FilePath
maybeAddSep [] = []
maybeAddSep p = if last p == pathSeparator then p else p ++ pathSeparatorStr

-- |Get the file path for this particular module.  In the IO monad
-- because it looks for the actual file.  Might eventually interface
-- with preprocessor libraries in order to correctly locate more
-- filenames.
-- Returns Nothing if the file doesn't exist.

moduleToFilePath :: FilePath -- ^search location
                 -> String   -- ^Module Name
                 -> [String] -- ^possible suffixes
                 -> IO (Maybe FilePath)

moduleToFilePath pref s possibleSuffixes
    = do let possiblePaths = moduleToPossiblePaths pref s possibleSuffixes
         matchList <- sequence $ map (\x -> do y <- doesFileExist x; return (x, y)) possiblePaths
         return $ listToMaybe [x | (x, True) <- matchList]

-- |Get the possible file paths based on this module name.
moduleToPossiblePaths :: FilePath -- ^search prefix
                      -> String -- ^module name
                      -> [String] -- ^possible suffixes
                      -> [FilePath]
moduleToPossiblePaths searchPref s possibleSuffixes
    =  let splitted = mySplit '.' s
           lastElem = last splitted
           pref = if (not $ null $ init splitted)
                  then maybeAddSep (pathJoin (init splitted))
                  else ""
        in [(maybeAddSep searchPref) ++ pref ++ x
             | x <- map (lastElem++) (map ("."++)possibleSuffixes)]



-- |Put the source files into the right directory in preperation for
-- something like sdist or installHugs.
moveSources :: FilePath -- ^build prefix (location of objects)
            -> FilePath -- ^Target directory
            -> [String] -- ^Modules
            -> [String] -- ^search suffixes
            -> IO ()
moveSources pref _targetDir sources searchSuffixes
    = do let targetDir = maybeAddSep _targetDir
         createIfNotExists True targetDir
	 -- Create parent directories for everything:
         sourceLocs <- sequence $ map moduleToFPErr sources
         let sourceLocsNoPref -- get rid of the prefix, for target location.
                 = if null pref then sourceLocs
                   else map (drop ((length pref) +1)) sourceLocs
	 mapM (createIfNotExists True)
		  $ nub [(removeFilename $ targetDir ++ x)
		   | x <- sourceLocsNoPref, (removeFilename x /= "")]
	 -- Put sources into place:
	 sequence_ [copyFile x (pathJoin [targetDir, y])
                      | (x,y) <- (zip sourceLocs sourceLocsNoPref)]
	 return ()
    where moduleToFPErr m
              = do p <- moduleToFilePath pref m searchSuffixes
                   when (isNothing p)
                            (putStrLn ("Error: Could not find module: " ++ m
                                       ++ " with any suffix: " ++ (show searchSuffixes))
                             >> exitWith (ExitFailure 1))
                   return $ fromJust p


mkLibName :: FilePath -- ^file Prefix
          -> String   -- ^library name.
          -> String
mkLibName pref lib = pathJoin [pref, ("libHS" ++ lib ++ ".a")]

-- | Create a path from a list of path elements
pathJoin :: [String] -> FilePath
pathJoin = concat . intersperse pathSeparatorStr

-- |Preserves permissions and, if possible, atime+mtime
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest 
    | dest == src = fail "copyFile: source and destination are the same file"
#ifndef __GLASGOW_HASKELL__
    | otherwise = do readFile src >>= writeFile dest
                     try (getPermissions src >>= setPermissions dest)
                     return ()
#else
    | otherwise = bracket (openBinaryFile src ReadMode) hClose $ \hSrc ->
                  bracket (openBinaryFile dest WriteMode) hClose $ \hDest ->
                  do allocaBytes bufSize $ \buffer -> copyContents hSrc hDest buffer
                     try (getPermissions src >>= setPermissions dest)
#ifdef HAVE_UNIX_PACKAGE
                     try $ do st <- getFileStatus src
                              let atime = accessTime st
                                  mtime = modificationTime st
                              setFileTimes dest atime mtime
#endif
                     return ()
  where bufSize = 1024
        copyContents hSrc hDest buffer
           = do count <- hGetBuf hSrc buffer bufSize
                when (count > 0) $ do hPutBuf hDest buffer count
                                      copyContents hSrc hDest buffer
#endif

partitionIO :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionIO f l
    = do bools <- sequence $ map f l
         let both = zip l bools
         return ([x | (x, True) <- both], [y | (y, False) <- both])

-- |Remove a list of files; if it encounters a directory, it doesn't
-- remove it, but returns it.  Throws everything that removeFile
-- throws unless the file is a directory.
removeFiles :: [FilePath]    -- ^Files and directories to remove
            -> IO [FilePath]
            {- ^The ones we were unable to remove because they were of
                an inappropriate type (directory) removeFiles -}
removeFiles files = liftM catMaybes (mapM rm' files)
      where
       rm' :: FilePath -> IO (Maybe FilePath)
       rm' f = do  temp <- try (removeFile f)
                   case temp of
                    Left e  -> do isDir <- doesDirectoryExist f
                                  -- If f is not a directory, re-throw the error
                                  unless isDir $ ioError e
                                  return (Just f)
                    Right _ -> return Nothing

-- |Probably follows symlinks, be careful.
removeFileRecursive :: FilePath -> IO ()
removeFileRecursive startLoc
    = do cont' <- getDirectoryContents startLoc
         let cont = filter (\x -> x /= "." && x /= "..") cont'
         curDir <- getCurrentDirectory
         setCurrentDirectory startLoc
         dirs <- removeFiles cont
         sequence $ map removeFileRecursive dirs
         setCurrentDirectory curDir
         removeDirectory startLoc

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests
    = let suffixes = ["hs", "lhs"]
          in [TestCase $
       do mp1 <- moduleToFilePath "" "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath "" "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   (Just "Distribution/Simple/Build.hs") mp1
          assertEqual "not existing not nothing failed" Nothing mp2,
       
        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo/Bar/Bang.hs","Foo/Bar/Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"]
        ]
#endif
