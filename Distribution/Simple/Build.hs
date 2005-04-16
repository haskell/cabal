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
			     		setupMessage, withLib,
                                        Executable(..), withExe,
                                        Library(..), libModules, hcOptions)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PreProcess (preprocessSources, PPSuffixHandler, ppCpp)
import Distribution.PreProcess.Unlit (unlit)
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Install (hugsMainFilename)
import Distribution.Simple.Utils (rawSystemExit, die, rawSystemPathExit,
                                  mkLibName, dotToSep,
				  moduleToFilePath, currentDir,
				  getOptionsFromSource, stripComments,
                                  smartCopySources
                                 )

import Data.Maybe(maybeToList)
import Control.Monad (unless, when)
#ifndef __NHC__
import Control.Exception (try)
#else
import IO (try)
#endif
import Data.List(nub, sort, isSuffixOf)
import System.Directory (removeFile)
import Distribution.Compat.Directory (copyFile,createDirectoryIfMissing)
import Distribution.Compat.FilePath (splitFilePath, joinFileName, joinFileExt,
				searchPathSeparator, objExtension, joinPaths, splitFileName)
import qualified Distribution.Simple.GHCPackageConfig
    as GHC (localPackageConfig, canReadLocalPackageConfig)

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Build the library

build :: PackageDescription
         -> LocalBuildInfo
         -> Int                 -- verbose
         -> [ PPSuffixHandler ]
         -> IO ()
build pkg_descr lbi verbose suffixes = do
  createDirectoryIfMissing True (buildDir lbi)
  preprocessSources pkg_descr lbi verbose suffixes
  setupMessage "Building" pkg_descr
  case compilerFlavor (compiler lbi) of
   GHC -> buildGHC pkg_descr lbi verbose
   Hugs -> buildHugs pkg_descr lbi verbose
   _   -> die ("Only building with GHC and preprocessing for hugs are implemented.")

-- |FIX: For now, the target must contain a main module.  Not used
-- ATM. Re-add later.
buildNHC :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
buildNHC pkg_descr lbi verbose = do
  -- Unsupported extensions have already been checked by configure
  let flags = snd $ extensionsToNHCFlag (maybe [] (extensions . libBuildInfo) (library pkg_descr))
  rawSystemExit verbose (compilerPath (compiler lbi))
                (["-nhc98"]
                ++ flags
                ++ maybe [] (hcOptions NHC . options . libBuildInfo) (library pkg_descr)
                ++ (libModules pkg_descr))

-- |Building for GHC.  If .ghc-packages exists and is readable, add
-- it to the command-line.
buildGHC :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
buildGHC pkg_descr lbi verbose = do
  let pref = buildDir lbi
  let ghcPath = compilerPath (compiler lbi)
  pkgConf <- GHC.localPackageConfig
  pkgConfReadable <- GHC.canReadLocalPackageConfig
  -- Build lib
  withLib pkg_descr () $ \lib -> do
      let libBi = libBuildInfo lib
          libTargetDir = pref `joinFileName` (hsSourceDir libBi)
      createDirectoryIfMissing True libTargetDir
      -- put hi-boot files into place for mutually recurive modules
      smartCopySources verbose (hsSourceDir libBi)
                       libTargetDir (libModules pkg_descr) ["hi-boot"] False
      let ghcArgs = ["-I" ++ dir | dir <- includeDirs libBi]
              ++ ["-optc" ++ opt | opt <- ccOptions libBi]
              ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
              ++ ["-package-name", pkgName (package pkg_descr),
                  "-odir",  libTargetDir,
                  "-hidir", libTargetDir
                 ]
              ++ constructGHCCmdLine Nothing libBi (packageDeps lbi)
              ++ (libModules pkg_descr)
              ++ (if verbose > 4 then ["-v"] else [])
      unless (null (libModules pkg_descr)) $
        rawSystemExit verbose ghcPath ghcArgs

      -- build any C sources
      unless (null (cSources libBi)) $
         sequence_ [do let odir = pref `joinFileName` dirOf c
                       createDirectoryIfMissing True odir
		       let cArgs = ["-I" ++ dir | dir <- includeDirs libBi]
			       ++ ["-optc" ++ opt | opt <- ccOptions libBi]
			       ++ ["-odir", odir, "-hidir", pref, "-c"]
			       ++ (if verbose > 4 then ["-v"] else [])
                       rawSystemExit verbose ghcPath (cArgs ++ [c])
                                   | c <- cSources libBi]

      -- link:
      let hObjs = [ (hsSourceDir libBi) `joinFileName` (dotToSep x) `joinFileExt` objExtension
                  | x <- libModules pkg_descr ]
          cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                  | (path, file, _) <- (map splitFilePath (cSources libBi)) ]
          libName  = mkLibName pref (showPackageId (package pkg_descr))

      stubObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") [objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat

      unless (null hObjs && null cObjs && null stubObjs) $ do
        try (removeFile libName) -- first remove library if it exists
        let arArgs = ["q"++ (if verbose > 4 then "v" else "")]
                ++ [libName]
                ++ [pref `joinFileName` x | x <- hObjs ++ cObjs]
                ++ stubObjs
        rawSystemPathExit verbose "ar" arArgs

  -- build any executables
  withExe pkg_descr $ \ (Executable exeName' modPath exeBi) -> do
                 createDirectoryIfMissing True (pref `joinFileName` (hsSourceDir exeBi))
		 let targetDir = pref `joinFileName` hsSourceDir exeBi
                 let exeDir = joinPaths targetDir (exeName' ++ "-tmp")
                 createDirectoryIfMissing True exeDir
                 -- put hi-boot files into place for mutually recurive modules
                 -- FIX: what about exeName.hi-boot?
                 smartCopySources verbose (hsSourceDir exeBi)
                                  exeDir (otherModules exeBi) ["hi-boot"] False

                 -- build executables
                 unless (null (cSources exeBi)) $
                  sequence_ [do let cSrcODir = exeDir `joinFileName` (fst $ splitFileName c)
                                createDirectoryIfMissing True cSrcODir
		                let cArgs = ["-I" ++ dir | dir <- includeDirs exeBi]
			                    ++ ["-optc" ++ opt | opt <- ccOptions exeBi]
			                    ++ ["-odir", cSrcODir, "-hidir", pref, "-c"]
			                    ++ (if verbose > 4 then ["-v"] else [])
                                rawSystemExit verbose ghcPath (cArgs ++ [c])
                                    | c <- cSources exeBi]

                 let cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                                   | (path, file, _) <- (map splitFilePath (cSources exeBi)) ]
                 let binArgs = ["-I" ++ dir | dir <- includeDirs exeBi]
                         ++ ["-optc" ++ opt | opt <- ccOptions exeBi]
                         ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
                         ++ ["-odir",  exeDir,
                             "-hidir", exeDir,
                             "-o",     targetDir `joinFileName` exeName'
                            ]
                         ++ constructGHCCmdLine (library pkg_descr >>= Just . hsSourceDir . libBuildInfo)
                                                exeBi (packageDeps lbi)
                         ++ [exeDir `joinFileName` x | x <- cObjs]
                         ++ [hsSourceDir exeBi `joinFileName` modPath]
			 ++ ldOptions exeBi
			 ++ ["-l"++lib | lib <- extraLibs exeBi]
			 ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
			 ++ (if verbose > 4 then ["-v"] else [])
                 rawSystemExit verbose ghcPath binArgs

dirOf :: FilePath -> FilePath
dirOf f = (\ (x, _, _) -> x) $ (splitFilePath f)

constructGHCCmdLine :: Maybe FilePath  -- If we're building an executable, we need the library's filepath
                    -> BuildInfo
                    -> [PackageIdentifier]
                    -> [String]
constructGHCCmdLine mSrcLoc bi deps = 
    -- Unsupported extensions have already been checked by configure
    let flags = snd $ extensionsToGHCFlag (extensions bi)
     in [ "--make", "-i" ++ hsSourceDir bi ]
     ++ maybe []  (\l -> ["-i" ++ l]) mSrcLoc
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
     ++ nub (flags ++ hcOptions GHC (options bi))
     ++ (concat [ ["-package", showPackageId pkg] | pkg <- deps ])

-- |Building a package for Hugs.
buildHugs :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
buildHugs pkg_descr lbi verbose = do
    let pref = buildDir lbi
    withLib pkg_descr () $ (\l -> compileBuildInfo pref Nothing (libModules pkg_descr) (libBuildInfo l))
    withExe pkg_descr $ compileExecutable (pref `joinFileName` "programs")
  where
	compileExecutable :: FilePath -> Executable -> IO ()
	compileExecutable destDir (exe@Executable {modulePath=mainPath, buildInfo=bi}) = do
            let exeMods = otherModules bi
	    let srcMainFile = hsSourceDir bi `joinFileName` mainPath
	    let exeDir = destDir `joinFileName` exeName exe
	    let destMainFile = exeDir `joinFileName` hugsMainFilename exe
	    copyModule (CPP `elem` extensions bi) bi srcMainFile destMainFile
	    compileBuildInfo exeDir (library pkg_descr >>= Just . hsSourceDir . libBuildInfo) exeMods bi
	    compileFFI bi destMainFile
	
	compileBuildInfo :: FilePath
                         -> Maybe FilePath -- ^The library source dir, if building exes
                         -> [String] -- ^Modules
                         -> BuildInfo -> IO ()
	compileBuildInfo destDir mLibSrcDir mods bi = do
	    -- Pass 1: copy or cpp files from src directory to build directory
	    let useCpp = CPP `elem` extensions bi
            let srcDir = hsSourceDir bi
	    let srcDirs = srcDir:(maybeToList mLibSrcDir)
            when (verbose > 3) (putStrLn $ "Source directories: " ++ show srcDirs)
	    fileLists <- sequence [moduleToFilePath srcDirs modu suffixes |
			modu <- mods]
	    let trimSrcDir
		  | null srcDir || srcDir == currentDir = id
		  | otherwise = drop (length srcDir + 1)
	    let copy_or_cpp f =
		    copyModule useCpp bi f (destDir `joinFileName` trimSrcDir f)
	    mapM_ copy_or_cpp (concat fileLists)
	    -- Pass 2: compile foreign stubs in build directory
	    stubsFileLists <- sequence [moduleToFilePath [destDir] modu suffixes |
			modu <- mods]
	    mapM_ (compileFFI bi) (concat stubsFileLists)

	suffixes = ["hs", "lhs"]

	-- Copy or cpp a file from the source directory to the build directory.
	copyModule :: Bool -> BuildInfo -> FilePath -> FilePath -> IO ()
	copyModule cppAll bi srcFile destFile = do
	    createDirectoryIfMissing True (dirOf destFile)
	    (exts, opts, _) <- getOptionsFromSource srcFile
	    let ghcOpts = hcOptions GHC opts
	    if cppAll || CPP `elem` exts || "-cpp" `elem` ghcOpts then do
	    	ppCpp bi lbi srcFile destFile verbose
	    	return ()
	      else
	    	copyFile srcFile destFile

	compileFFI :: BuildInfo -> FilePath -> IO ()
	compileFFI bi file = do
	    -- Only compile FFI stubs for a file if it contains some FFI stuff
	    inp <- readHaskellFile file
	    when ("foreign" `elem` symbols (stripComments False inp)) $ do
                when (verbose > 2) (putStrLn "Compiling FFI stubs")
		(_, opts, file_incs) <- getOptionsFromSource file
		let ghcOpts = hcOptions GHC opts
		let srcDir = hsSourceDir bi
		let pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes bi]
		let incs = uniq (sort (file_incs ++ includeOpts ghcOpts ++ pkg_incs))
		let pathFlag = "-P" ++ buildDir lbi ++ [searchPathSeparator]
		let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
		cfiles <- getCFiles file
		let cArgs =
			["-I" ++ dir | dir <- includeDirs bi] ++
			ccOptions bi ++
			map (joinFileName srcDir) cfiles ++
			["-L" ++ dir | dir <- extraLibDirs bi] ++
			ldOptions bi ++
			["-l" ++ lib | lib <- extraLibs bi] ++
			concat [["-framework", f] | f <- frameworks bi]
		rawSystemExit verbose ffihugs (hugsArgs ++ file : cArgs)

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

	-- List of terminal symbols in a source file.
	symbols :: String -> [String]
	symbols cs = case lex cs of
	    (sym, cs'):_ | not (null sym) -> sym : symbols cs'
	    _ -> []

	-- Get the non-literate source of a Haskell module.
	readHaskellFile :: FilePath -> IO String
	readHaskellFile file = do
	    text <- readFile file
	    return $ if ".lhs" `isSuffixOf` file then unlit file text else text

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (dropWhile (== x) xs)

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
