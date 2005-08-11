-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

{- Copyright (c) 2003-2005, Isaac Jones
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

import Distribution.Compiler (Compiler(..), CompilerFlavor(..),
				extensionsToGHCFlag, extensionsToNHCFlag)
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..),
			     		setupMessage, withLib,
                                        Executable(..), withExe,
                                        Library(..), libModules, hcOptions)
import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PreProcess (preprocessSources, PPSuffixHandler, ppCpp)
import Distribution.PreProcess.Unlit (unlit)
import Distribution.Version (Version(..))
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Install (hugsMainFilename)
import Distribution.Simple.Utils (rawSystemExit, die, rawSystemPathExit,
                                  mkLibName, mkProfLibName, mkGHCiLibName, dotToSep,
				  moduleToFilePath,
                                  smartCopySources,
                                  findFile
                                 )
import Language.Haskell.Extension (Extension(..))

import Data.Char(isSpace)
import Data.Maybe(mapMaybe, maybeToList)
import Control.Monad (unless, when, filterM)
#ifndef __NHC__
import Control.Exception (try)
#else
import IO (try)
#endif
import Data.List(nub, sort, isSuffixOf)
import System.Directory (removeFile)
import Distribution.Compat.Directory (copyFile,createDirectoryIfMissing)
import Distribution.Compat.FilePath (splitFilePath, joinFileName,
                                splitFileExt, joinFileExt, objExtension,
                                searchPathSeparator, joinPaths,
                                splitFileName, platformPath)
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
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = showPackageId (package pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

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
      ifProfLib = when (withProfLib lbi)
      ifGHCiLib = when (withGHCiLib lbi)
  pkgConf <- GHC.localPackageConfig
  pkgConfReadable <- GHC.canReadLocalPackageConfig
  -- Build lib
  withLib pkg_descr () $ \lib -> do
      let libBi = libBuildInfo lib
          libTargetDir = pref
      createDirectoryIfMissing True libTargetDir
      -- put hi-boot files into place for mutually recurive modules
      smartCopySources verbose (hsSourceDirs libBi)
                       libTargetDir (libModules pkg_descr) ["hi-boot"] False
      let ghcArgs = ["-I" ++ dir | dir <- includeDirs libBi]
              ++ ["-optc" ++ opt | opt <- ccOptions libBi]
              ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
              ++ ["-package-name", pkgName (package pkg_descr),
                  "-odir",  libTargetDir,
                  "-hidir", libTargetDir
                 ]
              ++ constructGHCCmdLine (compiler lbi) [] libBi (packageDeps lbi)
              ++ (libModules pkg_descr)
              ++ (if verbose > 4 then ["-v"] else [])
          ghcArgsProf = ghcArgs
              ++ ["-prof",
                  "-hisuf", "p_hi",
                  "-osuf", "p_o"
                 ]
              ++ ghcProfOptions libBi
      unless (null (libModules pkg_descr)) $
        do rawSystemExit verbose ghcPath ghcArgs
           ifProfLib (rawSystemExit verbose ghcPath ghcArgsProf)

      -- build any C sources
      unless (null (cSources libBi)) $
         sequence_ [do let ghc_vers = compilerVersion (compiler lbi)
			   odir | versionBranch ghc_vers >= [6,4,1] = pref
				| otherwise = pref `joinFileName` dirOf c
				-- ghc 6.4.1 fixed a bug in -odir handling
				-- for C compilations.
                       createDirectoryIfMissing True odir
		       let cArgs = ["-I" ++ dir | dir <- includeDirs libBi]
			       ++ ["-optc" ++ opt | opt <- ccOptions libBi]
			       ++ ["-odir", odir, "-hidir", pref, "-c"]
			       ++ (if verbose > 4 then ["-v"] else [])
                       rawSystemExit verbose ghcPath (cArgs ++ [c])
                                   | c <- cSources libBi]

      -- link:
      let hObjs = [ (dotToSep x) `joinFileExt` objExtension
                  | x <- libModules pkg_descr ]
          cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                  | (path, file, _) <- (map splitFilePath (cSources libBi)) ]
          libName  = mkLibName pref (showPackageId (package pkg_descr))
          hProfObjs = [ (dotToSep x) `joinFileExt` "p_"++objExtension
                      | x <- libModules pkg_descr ]
          profLibName  = mkProfLibName pref (showPackageId (package pkg_descr))
	  ghciLibName = mkGHCiLibName pref (showPackageId (package pkg_descr))

      stubObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") [objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat
      stubProfObjs <- sequence [moduleToFilePath [libTargetDir] (x ++"_stub") ["p_" ++ objExtension]
                           |  x <- libModules pkg_descr ]  >>= return . concat

      unless (null hObjs && null cObjs && null stubObjs) $ do
        try (removeFile libName) -- first remove library if it exists
        try (removeFile profLibName) -- first remove library if it exists
	try (removeFile ghciLibName) -- first remove library if it exists
        let arArgs = ["q"++ (if verbose > 4 then "v" else "")]
                ++ [libName]
                ++ [pref `joinFileName` x | x <- hObjs ++ cObjs]
                ++ stubObjs
            arProfArgs = ["q"++ (if verbose > 4 then "v" else "")]
                ++ [profLibName]
                ++ [pref `joinFileName` x | x <- hProfObjs ++ cObjs]
                ++ stubProfObjs
	    ldArgs = ["-r"]
                ++ ["-x"] -- FIXME: only some systems's ld support the "-x" flag
	        ++ ["-o", ghciLibName]
		++ [pref `joinFileName` x | x <- hObjs ++ cObjs]
		++ stubObjs
        rawSystemPathExit verbose "ar" arArgs
        ifProfLib (rawSystemPathExit verbose "ar" arProfArgs)
#if defined(mingw32_TARGET_OS) || defined(mingw32_HOST_OS)
        let (compilerDir, _) = splitFileName $ compilerPath (compiler lbi)
            (baseDir, _)     = splitFileName compilerDir
            ld = baseDir `joinFileName` "gcc-lib\\ld.exe"
        ifGHCiLib (rawSystemExit verbose ld ldArgs)
#else
        ifGHCiLib (rawSystemPathExit verbose "ld" ldArgs)
#endif

  -- build any executables
  withExe pkg_descr $ \ (Executable exeName' modPath exeBi) -> do
		 let targetDir = pref `joinFileName` exeName'
                 let exeDir = joinPaths targetDir (exeName' ++ "-tmp")
                 createDirectoryIfMissing True targetDir
                 createDirectoryIfMissing True exeDir
                 -- put hi-boot files into place for mutually recursive modules
                 -- FIX: what about exeName.hi-boot?
                 smartCopySources verbose (hsSourceDirs exeBi)
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
                 srcMainFile <- findFile (hsSourceDirs exeBi) modPath

                 let cObjs = [ path `joinFileName` file `joinFileExt` objExtension
                                   | (path, file, _) <- (map splitFilePath (cSources exeBi)) ]
                 let binArgs = ["-I" ++ dir | dir <- includeDirs exeBi]
                         ++ ["-optc" ++ opt | opt <- ccOptions exeBi]
                         ++ (if pkgConfReadable then ["-package-conf", pkgConf] else [])
                         ++ ["-odir",  exeDir,
                             "-hidir", exeDir,
                             "-o",     targetDir `joinFileName` exeName'
                            ]
                         ++ constructGHCCmdLine (compiler lbi) (maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr))
                                                exeBi (packageDeps lbi)
                         ++ [exeDir `joinFileName` x | x <- cObjs]
                         ++ [srcMainFile]
			 ++ ldOptions exeBi
			 ++ ["-l"++lib | lib <- extraLibs exeBi]
			 ++ ["-L"++libDir | libDir <- extraLibDirs exeBi]
			 ++ (if verbose > 4 then ["-v"] else [])
                         ++ if withProfExe lbi
                               then "-prof":ghcProfOptions exeBi
                               else []
                 rawSystemExit verbose ghcPath binArgs

dirOf :: FilePath -> FilePath
dirOf f = (\ (x, _, _) -> x) $ (splitFilePath f)

constructGHCCmdLine :: Compiler
                    -> [FilePath]  -- If we're building an executable, we need the library's filepath
                    -> BuildInfo
                    -> [PackageIdentifier]
                    -> [String]
constructGHCCmdLine comp srcLocs bi deps = 
    -- Unsupported extensions have already been checked by configure
    let flags = snd $ extensionsToGHCFlag (extensions bi)
     in (if compilerVersion comp > Version [6,4] []
            then ["-hide-all-packages"]
            else [])
     ++ ["--make"]
     ++ ["-i"]
     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi ++ srcLocs)]
     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
     ++ nub (flags ++ hcOptions GHC (options bi))
     ++ (concat [ ["-package", showPackageId pkg] | pkg <- deps ])

-- |Building a package for Hugs.
buildHugs :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
buildHugs pkg_descr lbi verbose = do
    let pref = buildDir lbi
    withLib pkg_descr () $ (\l -> compileBuildInfo pref [] (libModules pkg_descr) (libBuildInfo l))
    withExe pkg_descr $ compileExecutable (pref `joinFileName` "programs")
  where
	compileExecutable :: FilePath -> Executable -> IO ()
	compileExecutable destDir (exe@Executable {modulePath=mainPath, buildInfo=bi}) = do
            let exeMods = otherModules bi
	    srcMainFile <- findFile (hsSourceDirs bi) mainPath
	    let exeDir = destDir `joinFileName` exeName exe
	    let destMainFile = exeDir `joinFileName` hugsMainFilename exe
	    copyModule (CPP `elem` extensions bi) bi srcMainFile destMainFile
	    compileBuildInfo exeDir (maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr)) exeMods bi
	    compileFiles bi [destMainFile]
	
	compileBuildInfo :: FilePath
                         -> [FilePath] -- ^library source dirs, if building exes
                         -> [String] -- ^Modules
                         -> BuildInfo -> IO ()
	compileBuildInfo destDir mLibSrcDirs mods bi = do
	    -- Pass 1: copy or cpp files from src directory to build directory
	    let useCpp = CPP `elem` extensions bi
	    let srcDirs = nub $ hsSourceDirs bi ++ mLibSrcDirs
            when (verbose > 3) (putStrLn $ "Source directories: " ++ show srcDirs)
            flip mapM_ mods $ \ m -> do
                fs <- moduleToFilePath srcDirs m suffixes
                if null fs then
                    die ("can't find source for module " ++ m)
                  else do
                    let srcFile = head fs
                    let (_, ext) = splitFileExt srcFile
                    copyModule useCpp bi srcFile
                        (destDir `joinFileName` dotToSep m `joinFileExt` ext)
	    -- Pass 2: compile foreign stubs in build directory
	    stubsFileLists <- sequence [moduleToFilePath [destDir] modu suffixes |
			modu <- mods]
            compileFiles bi (concat stubsFileLists)

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

        compileFiles :: BuildInfo -> [FilePath] -> IO ()
        compileFiles bi fileList = do
	    ffiFileList <- filterM testFFI fileList
            unless (null ffiFileList) $ do
                when (verbose > 2) (putStrLn "Compiling FFI stubs")
	        mapM_ (compileFFI bi) ffiFileList

        -- Only compile FFI stubs for a file if it contains some FFI stuff
        testFFI :: FilePath -> IO Bool
        testFFI file = do
            inp <- readHaskellFile file
            return ("foreign" `elem` symbols (stripComments False inp))

        compileFFI :: BuildInfo -> FilePath -> IO ()
        compileFFI bi file = do
            (_, opts, file_incs) <- getOptionsFromSource file
            let ghcOpts = hcOptions GHC opts
            let pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes bi]
            let incs = uniq (sort (file_incs ++ includeOpts ghcOpts ++ pkg_incs))
            let pathFlag = "-P" ++ buildDir lbi ++ [searchPathSeparator]
            let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
            cfiles <- getCFiles file
            let cArgs =
                    ["-I" ++ dir | dir <- includeDirs bi] ++
                    ccOptions bi ++
                    cfiles ++
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
	    return [platformPath cfile |
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
-- * options in source files
-- ------------------------------------------------------------

-- |Read the initial part of a source file, before any Haskell code,
-- and return the contents of any LANGUAGE, OPTIONS and INCLUDE pragmas.
getOptionsFromSource
    :: FilePath
    -> IO ([Extension],                 -- LANGUAGE pragma, if any
           [(CompilerFlavor,[String])], -- OPTIONS_FOO pragmas
           [String]                     -- INCLUDE pragmas
          )
getOptionsFromSource file = do
    text <- readFile file
    return $ foldr appendOptions ([],[],[]) $ map getOptions $
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

	getOptions ("OPTIONS":opts) = ([], [(GHC, opts)], [])
	getOptions ("OPTIONS_GHC":opts) = ([], [(GHC, opts)], [])
	getOptions ("OPTIONS_NHC98":opts) = ([], [(NHC, opts)], [])
	getOptions ("OPTIONS_HUGS":opts) = ([], [(Hugs, opts)], [])
	getOptions ("LANGUAGE":ws) = (mapMaybe readExtension ws, [], [])
	  where	readExtension :: String -> Maybe Extension
		readExtension w = case reads w of
		    [(ext, "")] -> Just ext
		    [(ext, ",")] -> Just ext
		    _ -> Nothing
	getOptions ("INCLUDE":ws) = ([], [], ws)
	getOptions _ = ([], [], [])

	appendOptions (exts, opts, incs) (exts', opts', incs')
          = (exts++exts', opts++opts', incs++incs')

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
	stripCommentsLevel 0 ('"':cs) = '"':copyString cs
	stripCommentsLevel 0 ('-':'-':cs) =	-- FIX: symbols like -->
	    stripCommentsLevel 0 (dropWhile (/= '\n') cs)
	stripCommentsLevel 0 ('{':'-':'#':cs)
	  | keepPragmas = '{' : '-' : '#' : copyPragma cs
	stripCommentsLevel n ('{':'-':cs) = stripCommentsLevel (n+1) cs
	stripCommentsLevel 0 (c:cs) = c : stripCommentsLevel 0 cs
	stripCommentsLevel n ('-':'}':cs) = stripCommentsLevel (n-1) cs
	stripCommentsLevel n (c:cs) = stripCommentsLevel n cs
	stripCommentsLevel _ [] = []

	copyString ('\\':c:cs) = '\\' : c : copyString cs
	copyString ('"':cs) = '"' : stripCommentsLevel 0 cs
	copyString (c:cs) = c : copyString cs
	copyString [] = []

	copyPragma ('#':'-':'}':cs) = '#' : '-' : '}' : stripCommentsLevel 0 cs
	copyPragma (c:cs) = c : copyPragma cs
	copyPragma [] = []

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
