{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
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

module Distribution.Simple.Install (
	install,
	mkImportDir
  ) where

import Distribution.Package
import Distribution.Simple.Configure(LocalBuildInfo(..))
import Distribution.Simple.Utils(setupMessage)

import Data.List(inits, nub, intersperse, findIndices)
import System.Cmd(system)
import System.Directory(doesDirectoryExist, createDirectory)
import System.Exit


-- |FIX: for now, only works with hugs or sdist-style
-- installation... must implement for .hi files and such...  how do we
-- know which files to expect?
install :: PackageDescription -> LocalBuildInfo -> IO ()
install pkg_descr lbi = do
  setupMessage "Installing" pkg_descr
  moveSources (prefix lbi) (allModules pkg_descr) (mainModules pkg_descr)
  exitWith (ExitFailure 1)

-- -----------------------------------------------------------------------------
-- Installation policies

mkImportDir :: PackageDescription -> LocalBuildInfo -> FilePath
mkImportDir pkg_descr lbi = 
#ifdef mingw32_TARGET_OS
	prefix lbi ++ '/':pkg_name
#else
	prefix lbi ++ "/lib/" ++ pkg_name
#endif
  where 
	pkg_name = showPackageId (package pkg_descr)

-- |Put the source files into the right directory in preperation for
-- something like sdist or installHugs.
moveSources :: FilePath   -- ^Target directory
            -> [FilePath] -- ^sources
            -> [FilePath] -- ^Main modules
            -> IO ()
moveSources _targetDir sources mains
    = do let targetDir = maybeAddSep _targetDir
         createIfNotExists True targetDir
	 -- Create parent directories for everything:
	 mapM (createIfNotExists True)
		  $ nub [(removeFilename $ targetDir ++ x)
		   | x <- (sources ++ mains), (removeFilename x /= "")]
	 -- Put sources into place:
	 mapM system ["cp -r " ++ x ++ " " ++ targetDir ++ x
				  | x <- sources ++ mains]
	 return ()

-- ------------------------------------------------------------
-- * utility functions
-- ------------------------------------------------------------

-- |FIX: Do we actually have to make something differnet for windows,
-- or does this work?
pathSeperator = '/'
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
maybeAddSep p = if last p == pathSeperator then p else p ++ pathSeperatorStr
