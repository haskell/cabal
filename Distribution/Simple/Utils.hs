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
	splitFilePath,
	joinFilenameDir,
        joinExt,
        pathInits,
	isPathSeparator,
        pathSeparatorStr,
        split,
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
        withLib,
#ifdef DEBUG
        hunitTests
#endif
  ) where

#if __GLASGOW_HASKELL__ < 603 
#include "config.h"
#endif

import Distribution.Package (PackageDescription(..), showPackageId,
                             BuildInfo(..), hasLibs)

import Control.Monad(when, unless, liftM, mapM)
import Data.List(nub, intersperse, findIndices)
import Data.Maybe(Maybe, listToMaybe, isNothing, fromJust, catMaybes)
import System.IO (hPutStr, stderr, hFlush, stdout
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
import System.Posix.Files (getFileStatus, accessTime, modificationTime, setFileTimes, fileMode, setFileMode)

#ifdef DEBUG
import HUnit ((~:), (~=?), Test(..), assertEqual)
#endif

-- -----------------------------------------------------------------------------
-- Pathname-related utils

-- | Split the path into (directory, filename sans extension, extension)
splitFilePath :: FilePath -> (String, String, String)
splitFilePath p =
  case pre of
    []       -> (reverse real_dir, reverse suf, [])
    (_:pre') -> (reverse real_dir, reverse pre', reverse suf)
  where
#ifdef mingw32_TARGET_OS
    (path,drive) = break (== ':') (reverse p)
#else
    (path,drive) = (reverse p,"")
#endif
    (file,dir)   = break isPathSeparator path
    (suf,pre)    = case file of
                     ".." -> ("..", "")
                     _    -> break (== '.') file
    
    real_dir = case dir of
      []       -> "."++drive
      [_]      -> pathSeparatorStr++drive
      (_:dir') -> dir'++drive

-- | Join extension to file path
joinExt :: FilePath -> String -> String
joinExt path ""  = path
joinExt path ext = path++'.':ext

-- |Not exported.  Does this file have the given extension?
hasExt :: FilePath -- ^Does this file
       -> String   -- ^Have this extension?
       -> Bool
hasExt f testExt = let (_, _, realExt) = splitFilePath f
                       in testExt == realExt

-- Join file name to directory
joinFilenameDir :: String -> String -> FilePath
joinFilenameDir dir ""    = dir
joinFilenameDir dir fname = dir++pathSeparator:fname

-- |Get this path and all its parents.
pathInits :: FilePath -> [FilePath]
pathInits p =
    map ((++) root') (dropEmptyPath $ inits path')
    where
#ifdef mingw32_TARGET_OS
       (root,path) = case break (== ':') p of
          (path,    "") -> ("",path)
          (root,_:path) -> (root++":",path)
#else
       (root,path) = ("",p)
#endif
       (root',path') = case path of
         (c:path'') | isPathSeparator c -> (root++pathSeparatorStr,path'')
         _                              -> (root,path)
         
       dropEmptyPath ("":paths) = paths
       dropEmptyPath paths      = paths

       inits :: String -> [String]
       inits [] =  [""]
       inits cs = 
         case pre of
           "."  -> inits suf
           ".." -> map (joinFilenameDir pre) (dropEmptyPath $ inits suf)
           _    -> "" : map (joinFilenameDir pre) (inits suf)
         where
           (pre,suf) = case break isPathSeparator cs of
              (pre',"")    -> (pre', "")
              (pre',_:suf') -> (pre',suf')

isPathSeparator :: Char -> Bool
isPathSeparator ch =
#ifdef mingw32_TARGET_OS
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif

pathSeparator :: Char
#ifdef mingw32_TARGET_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

pathSeparatorStr :: String
pathSeparatorStr = [pathSeparator]

split :: Char -> String -> [String]
split c s = case rest of
		[]     -> [chunk] 
		_:rest' -> chunk : split c rest'
  where (chunk, rest) = break (==c) s

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
die msg = do hFlush stdout; hPutStr stderr (msg++"\n"); exitWith (ExitFailure 1)

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
maybeAddSep p  = if isPathSeparator (last p) then p else p ++ pathSeparatorStr

-- |If this filename ends in the path separator, remove it.
maybeRemoveSep :: FilePath -> FilePath
maybeRemoveSep [] = []
maybeRemoveSep p  = if isPathSeparator (last p) then init p else p

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


copyPermissions src dest
    = do srcStatus <- getFileStatus src
         setFileMode dest (fileMode srcStatus)

copyFileTimes :: FilePath -> FilePath -> IO ()
#ifndef mingw32_TARGET_OS
copyFileTimes src dest
   = do st <- getFileStatus src
	let atime = accessTime st
	    mtime = modificationTime st
	setFileTimes dest atime mtime
#else
copyFileTimes src dest
    = return ()
#endif

-- |Preserves permissions and, if possible, atime+mtime
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest 
    | dest == src = fail "copyFile: source and destination are the same file"
#if (!(defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 600))
    | otherwise = do readFile src >>= writeFile dest
                     try (copyPermissions src dest)
                     return ()
#else
    | otherwise = bracket (openBinaryFile src ReadMode) hClose $ \hSrc ->
                  bracket (openBinaryFile dest WriteMode) hClose $ \hDest ->
                  do allocaBytes bufSize $ \buffer -> copyContents hSrc hDest buffer
                     try (copyPermissions src dest)
                     try (copyFileTimes src dest)
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

-- |Might want to make this more generic some day, with regexps
-- or something.
filesWithExtensions :: FilePath -- ^Directory to look in
                    -> String   -- ^The extension
                    -> IO [FilePath] {- ^The file names (not full
                                     path) of all the files with this
                                     extension in this directory. -}
filesWithExtensions dir extension 
    = do allFiles <- getDirectoryContents dir
         return $ filter ((flip hasExt) extension) allFiles


-- |If the package description has a library section, call the given
--  function with the library build info as argument.
withLib :: PackageDescription -> (BuildInfo -> IO ()) -> IO ()
withLib pkg_descr f = when (hasLibs pkg_descr) $ f (fromJust (library pkg_descr))

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
              
        TestLabel "splitFilePath" $ TestList 
           ["simpleCase"   ~: ("c:\\foo",   "bar", "txt") ~=? (splitFilePath "c:\\foo\\bar.txt"),
            "dotInDirName" ~: ("\\foo.txt", "bar",    "") ~=? (splitFilePath "\\foo.txt\\bar"),
            "justName"     ~: (".",         "bar",    "") ~=? (splitFilePath "bar"),
            "justExt"      ~: (".",            "", "txt") ~=? (splitFilePath ".txt"),
            "rootDir"      ~: ("\\",        "foo",    "") ~=? (splitFilePath "\\foo"),
            "noFile"       ~: ("\\foo\\bar",   "",    "") ~=? (splitFilePath "\\foo\\bar\\"),
            "parentDir"    ~: (".",          "..",    "") ~=? (splitFilePath ".."),
            "curDir"       ~: (".",            "",    "") ~=? (splitFilePath "."),
	    "root"         ~: ("\\",           "",    "") ~=? (splitFilePath "\\"),
	    "curDirDrive"  ~: ("c:.",          "",    "") ~=? (splitFilePath "c:."),
	    "rootDrive1"   ~: ("c:\\",         "",    "") ~=? (splitFilePath "c:\\"),
	    "rootDrive2"   ~: ("c:.",          "",    "") ~=? (splitFilePath "c:"),
	    "rootDrive2"   ~: ("c:.",      "test", "txt") ~=? (splitFilePath "c:test.txt")
	   ],
        TestLabel "joinFilenameDir&joinExt" $ TestList
           ["simpleCase"   ~: ("\\foo\\bar.txt") ~=? ("\\foo" `joinFilenameDir` ("bar" `joinExt` "txt")),
            "justDir"      ~: ("\\foo")          ~=? ("\\foo" `joinFilenameDir` (""    `joinExt` "")),
            "justExt"      ~: (".\\.txt")        ~=? ("."     `joinFilenameDir` (""    `joinExt` "txt")),
            "curDir"       ~: (".")              ~=? ("."     `joinFilenameDir` (""    `joinExt` "")),
            "root"         ~: ("\\")             ~=? ("\\"    `joinFilenameDir` (""    `joinExt` ""))
	   ],

	TestLabel "pathInits" $ TestList
           ["simpleCase"    ~: ["c:\\foo","c:\\foo\\bar.txt"] ~=? (pathInits "c:\\foo\\bar.txt"),
            "justName"      ~: ["bar.txt"] ~=? (pathInits "bar.txt"),
            "driveAndName1" ~: ["c:bar.txt"] ~=? (pathInits "c:bar.txt"),
            "driveAndName2" ~: ["c:\\bar.txt"] ~=? (pathInits "c:\\bar.txt"),
            "locDir"        ~: ["bar.txt"] ~=? (pathInits ".\\bar.txt"),
            "midLocDir"     ~: ["foo","foo\\bar.txt"] ~=? (pathInits "foo\\.\\bar.txt"),
            "withParentDir1"~: ["..\\foo"] ~=? (pathInits "..\\foo"),
            "withParentDir2"~: ["foo\\..\\bar", "foo\\..\\bar\\baz"] ~=? (pathInits "foo\\..\\bar\\baz"),
            "parentDir"     ~: [] ~=? (pathInits ".."),
            "rootFile"      ~: ["\\bar.txt"] ~=? (pathInits "\\bar.txt"),
            "curDir"        ~: [] ~=? (pathInits "."),
            "root"          ~: [] ~=? (pathInits "\\")
	   ]
#else
       do mp1 <- moduleToFilePath "" "Distribution.Simple.Build" suffixes --exists
          mp2 <- moduleToFilePath "" "Foo.Bar" suffixes    -- doesn't exist
          assertEqual "existing not found failed"
                   (Just "Distribution/Simple/Build.hs") mp1
          assertEqual "not existing not nothing failed" Nothing mp2,

        "moduleToPossiblePaths 1" ~: "failed" ~:
             ["Foo/Bar/Bang.hs","Foo/Bar/Bang.lhs"]
                ~=? (moduleToPossiblePaths "" "Foo.Bar.Bang" suffixes),
        "moduleToPossiblePaths2 " ~: "failed" ~:
              (moduleToPossiblePaths "" "Foo" suffixes) ~=? ["Foo.hs", "Foo.lhs"],

        TestLabel "splitFilePath" $ TestList 
           ["simpleCase"   ~: ("/foo",     "bar", "txt") ~=? (splitFilePath "/foo/bar.txt"),
            "dotInDirName" ~: ("/foo.txt", "bar",    "") ~=? (splitFilePath "/foo.txt/bar"),
            "justName"     ~: (".",        "bar",    "") ~=? (splitFilePath "bar"),
            "justExt"      ~: (".",           "", "txt") ~=? (splitFilePath ".txt"),
            "rootDir"      ~: ("/",        "foo",    "") ~=? (splitFilePath "/foo"),
            "noFile"       ~: ("/foo/bar",    "",    "") ~=? (splitFilePath "/foo/bar/"),
            "parentDir"    ~: (".",         "..",    "") ~=? (splitFilePath ".."),
            "curDir"       ~: (".",           "",    "") ~=? (splitFilePath "."),
	    "root"         ~: ("/",           "",    "") ~=? (splitFilePath "/"),
            "hasExt"       ~: True                       ~=? (hasExt "foo/bang.hs" "hs")
	   ],

        TestLabel "joinFilenameDir&joinExt" $ TestList
           ["simpleCase"   ~: ("/foo/bar.txt") ~=? ("/foo" `joinFilenameDir` ("bar" `joinExt` "txt")),
            "justDir"      ~: ("/foo")         ~=? ("/foo" `joinFilenameDir` (""    `joinExt` "")),
            "justExt"      ~: ("./.txt")       ~=? ("."    `joinFilenameDir` (""    `joinExt` "txt")),
            "curDir"       ~: (".")            ~=? ("."    `joinFilenameDir` (""    `joinExt` "")),
            "root"         ~: ("/")            ~=? ("/"    `joinFilenameDir` (""    `joinExt` ""))
	   ],

	TestLabel "pathInits" $ TestList
           ["simpleCase"    ~: ["/foo","/foo/bar.txt"] ~=? (pathInits "/foo/bar.txt"),
            "justName"      ~: ["bar.txt"] ~=? (pathInits "bar.txt"),
            "locDir"        ~: ["bar.txt"] ~=? (pathInits "./bar.txt"),
            "midLocDir"     ~: ["foo","foo/bar.txt"] ~=? (pathInits "foo/./bar.txt"),
            "rootFile"      ~: ["/bar.txt"] ~=? (pathInits "/bar.txt"),
            "withParentDir1"~: ["../foo"] ~=? (pathInits "../foo"),
            "withParentDir2"~: ["foo/../bar", "foo/../bar/baz"] ~=? (pathInits "foo/../bar/baz"),
            "parentDir"     ~: [] ~=? (pathInits ".."),
            "curDir"        ~: [] ~=? (pathInits "."),
            "root"          ~: [] ~=? (pathInits "/")
	   ],
        TestCase (do files <- filesWithExtensions "." "description"
                     assertEqual "filesWithExtensions" "Setup.description" (head files))
#endif
          ]
#endif
