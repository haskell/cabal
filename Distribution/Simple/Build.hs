-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

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

module Distribution.Simple.Build (
	build
#ifdef DEBUG        
        ,hunitTests
#endif
  ) where

import Distribution.Extension (Extension(..),
				extensionsToGHCFlag, extensionsToNHCFlag)
import Distribution.Setup (Compiler(..), CompilerFlavor(..))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..), 
			     		setupMessage, withLib, Executable(..),
                                        libModules, biModules)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PreProcess (preprocessSources, PPSuffixHandler)
import Distribution.PreProcess.Unlit (unlit)
import Distribution.Simple.Configure (LocalBuildInfo(..), compiler, exeDeps)
import Distribution.Simple.Install (hugsMainFilename)
import Distribution.Simple.Utils (rawSystemExit, die, rawSystemPathExit,
                                  createIfNotExists,
                                  mkLibName, moveSources, dotToSep,
				  moduleToFilePath, currentDir,
				  getOptionsFromSource, stripComments
                                 )


import Control.Monad (unless, when)
import Control.Exception (try)
import Data.Char (isAlpha, isAlphaNum)
import Data.List(nub, sort, isSuffixOf)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import Distribution.Compat.Directory (copyFile)
import Distribution.Compat.FilePath (splitFilePath, joinFileName, joinFileExt,
				searchPathSeparator)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canReadLocalPackageConfig)

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Build the library

build :: PackageDescription
         -> LocalBuildInfo
         -> [ PPSuffixHandler ]
         -> IO ()
build pkg_descr lbi suffixes = do
  createIfNotExists True (buildDir lbi)
  preprocessSources pkg_descr lbi suffixes
  setupMessage "Building" pkg_descr
  case compilerFlavor (compiler lbi) of
   GHC -> buildGHC pkg_descr lbi
   Hugs -> buildHugs pkg_descr lbi
   _   -> die ("Only building with GHC and preprocessing for hugs are implemented.")

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
buildNHC :: PackageDescription -> LocalBuildInfo -> IO ()
buildNHC pkg_descr lbi = do
  -- Unsupported extensions have already been checked by configure
  let flags = snd $ extensionsToNHCFlag (maybe [] extensions (library pkg_descr))
  rawSystemExit (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ flags
                ++ [ opt | (NHC,opts) <- maybe [] options (library pkg_descr),
                           opt <- opts ]
                ++ (libModules pkg_descr))

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
buildGHC :: PackageDescription -> LocalBuildInfo -> IO ()
buildGHC pkg_descr lbi = do
  let pref = buildDir lbi
  let ghcPath = compilerPath (compiler lbi)
  pkgConf <- GHC.localPackageConfig
  pkgConfReadable <- GHC.canReadLocalPackageConfig
  -- Build lib
  withLib pkg_descr $ \buildInfo' -> do
      createIfNotExists True (pref `joinFileName` (hsSourceDir buildInfo'))
      let args = ccOptions pkg_descr
              ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
              ++ ["-package-name", pkgName (package pkg_descr),
                  "-odir",  pref `joinFileName` (hsSourceDir buildInfo'),
                  "-hidir", pref `joinFileName` (hsSourceDir buildInfo')
                 ]
              ++ constructGHCCmdLine buildInfo' (packageDeps lbi)
              ++ (exposedModules buildInfo' ++ hiddenModules buildInfo')
      unless (null (hiddenModules buildInfo' ++ exposedModules buildInfo')) $
        rawSystemExit ghcPath args

      -- build any C sources
      unless (null (cSources buildInfo')) $
         sequence_ [do let odir = pref `joinFileName` dirOf c
                       createIfNotExists True odir
		       let args = ccOptions pkg_descr
			       ++ ["-odir", odir, "-hidir", pref, "-c"]
                       rawSystemExit ghcPath (args ++ [c])
                                   | c <- cSources buildInfo']

      -- link:
      let hObjs = [ (hsSourceDir buildInfo') `joinFileName` (dotToSep x) `joinFileExt` objsuffix
                  | x <- exposedModules buildInfo' ++ hiddenModules buildInfo' ]
          cObjs = [ path `joinFileName` file `joinFileExt` objsuffix
                  | (path, file, _) <- (map splitFilePath (cSources buildInfo')) ]
          lib  = mkLibName pref (showPackageId (package pkg_descr))
      unless (null hObjs && null cObjs) $ do
        try (removeFile lib) -- first remove library if it exists
        rawSystemPathExit "ar" (["q", lib] ++ [pref `joinFileName` x | x <- hObjs ++ cObjs])

  -- build any executables
  sequence_ [ do createIfNotExists True (pref `joinFileName` (hsSourceDir exeBi))
		 let targetDir = pref `joinFileName` hsSourceDir exeBi
                 let args = ccOptions pkg_descr
                         ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
                         ++ ["-odir",  targetDir,
                             "-hidir", targetDir,
                             "-o",     targetDir `joinFileName` exeName'
                            ]
                         ++ constructGHCCmdLine exeBi (exeDeps exeName' lbi)
                         ++ [hsSourceDir exeBi `joinFileName` modPath]
			 ++ ldOptions pkg_descr
                 rawSystemExit ghcPath args
             | Executable exeName' modPath exeBi <- executables pkg_descr]

dirOf :: FilePath -> FilePath
dirOf f = (\ (x, _, _) -> x) $ (splitFilePath f)

constructGHCCmdLine :: BuildInfo -> [PackageIdentifier] -> [String]
constructGHCCmdLine buildInfo' deps = 
    -- Unsupported extensions have already been checked by configure
    let flags = snd $ extensionsToGHCFlag (extensions buildInfo')
     in [ "--make", "-i" ++ hsSourceDir buildInfo' ]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes buildInfo' ]
     ++ nub (flags ++ [ opt | (GHC,opts) <- options buildInfo', opt <- opts ])
     ++ (concat [ ["-package", pkgName pkg] | pkg <- deps ])

-- |
buildHugs :: PackageDescription -> LocalBuildInfo -> IO ()
buildHugs pkg_descr lbi = do
    let pref = buildDir lbi
    withLib pkg_descr $ compileBuildInfo pref
    mapM_ (compileExecutable (pref `joinFileName` "programs"))
	(executables pkg_descr)
  where
	compileExecutable :: FilePath -> Executable -> IO ()
	compileExecutable destDir (exe@Executable {modulePath=mainPath, buildInfo=bi}) = do
	    let srcMainFile = hsSourceDir bi `joinFileName` mainPath
	    let destMainFile = destDir `joinFileName` hugsMainFilename exe
	    copyModule (CPP `elem` extensions bi) srcMainFile destMainFile
	    compileBuildInfo destDir bi
	    compileFFI bi destMainFile
	
	compileBuildInfo :: FilePath -> BuildInfo -> IO ()
	compileBuildInfo destDir bi = do
	    -- Pass 1: copy or cpp files from src directory to build directory
	    let useCpp = CPP `elem` extensions bi
	    let srcDir = hsSourceDir bi
	    fileLists <- sequence [moduleToFilePath srcDir mod suffixes |
			mod <- biModules bi]
	    let trimSrcDir
		  | null srcDir || srcDir == currentDir = id
		  | otherwise = drop (length srcDir + 1)
	    let copy_or_cpp f =
		    copyModule useCpp f (destDir `joinFileName` trimSrcDir f)
	    mapM_ copy_or_cpp (concat fileLists)
	    -- Pass 2: compile foreign stubs in build directory
	    fileLists <- sequence [moduleToFilePath destDir mod suffixes |
			mod <- biModules bi]
	    mapM_ (compileFFI bi) (concat fileLists)

	suffixes = ["hs", "lhs"]

	-- Copy or cpp a file from the source directory to the build directory.
	copyModule :: Bool -> FilePath -> FilePath -> IO ()
	copyModule cppAll srcFile destFile = do
	    createIfNotExists True (dirOf destFile)
	    (exts, opts) <- getOptionsFromSource srcFile
	    let ghcOpts = hcOptions GHC opts
	    if cppAll || CPP `elem` exts || "-cpp" `elem` ghcOpts then
	    	cppFile srcFile destFile
	      else
	    	copyFile srcFile destFile

	{- FIX (HUGS): assumes gcc -}
	cppFile inFile outFile =
	    rawSystemExit "cpp"
		(["-traditional", "-P", "-D__HUGS__"] ++
			ccOptions pkg_descr ++ [inFile, outFile])

	compileFFI :: BuildInfo -> FilePath -> IO ()
	compileFFI bi file = do
	    -- Only compile FFI stubs for a file if it contains some FFI stuff
	    inp <- readHaskellFile file
	    when ("foreign" `elem` identifiers (stripComments False inp)) $ do
		(_, opts) <- getOptionsFromSource file
		let ghcOpts = hcOptions GHC opts
		let srcDir = hsSourceDir bi
		let pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes bi]
		let incs = uniq (sort (includeOpts ghcOpts ++ pkg_incs))
		let pathFlag = "-P" ++ buildDir lbi ++ [searchPathSeparator]
		let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
		cfiles <- getCFiles file
		let cArgs =
			ccOptions pkg_descr ++
			map (joinFileName srcDir) cfiles ++
			["-L" ++ dir | dir <- extraLibDirs bi] ++
			ldOptions pkg_descr ++
			["-l" ++ lib | lib <- extraLibs bi] ++
			concat [["-framework", f] | f <- frameworks pkg_descr]
		rawSystemExit ffihugs (hugsArgs ++ file : cArgs)

	ffihugs = compilerPath (compiler lbi)

	includeOpts :: [String] -> [String]
	includeOpts [] = []
	includeOpts ("-#include" : arg : opts) = arg : includeOpts opts
	includeOpts (_ : opts) = includeOpts opts

	-- get C file names from CFILES pragmas throughout the source file
	getCFiles :: FilePath -> IO [String]
	getCFiles file = do
	    inp <- readHaskellFile file
	    return [cfile |
		"{-#" : "CFILES" : rest <-
			map words $ lines $ stripComments True inp,
		last rest == "#-}",
		cfile <- init rest]

	-- List of variable identifiers (and reserved words) in a source file.
	identifiers :: String -> [String]
	identifiers cs = case dropWhile (not . isStartChar) cs of
	    [] -> []
	    rest -> ident : identifiers cs'
	      where (ident, cs') = span isFollowChar rest

	isStartChar c = c == '_' || isAlpha c
	isFollowChar c = c == '_' || c == '\'' || isAlphaNum c

	-- Get the non-literate source of a Haskell module.
	readHaskellFile :: FilePath -> IO String
	readHaskellFile file = do
	    text <- readFile file
	    return $ if ".lhs" `isSuffixOf` file then unlit file text else text

hcOptions :: CompilerFlavor -> [(CompilerFlavor, [String])] -> [String]
hcOptions hc hc_opts = [opt | (hc',opts) <- hc_opts, hc' == hc, opt <- opts]

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (dropWhile (== x) xs)

objsuffix :: String
objsuffix = "o"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
