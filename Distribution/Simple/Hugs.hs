{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Hugs
-- Copyright   :  Isaac Jones 2003-2006
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Build and install functionality for Hugs.

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

module Distribution.Simple.Hugs (
	build, install
 ) where

import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions, autogenModuleName )
import Distribution.Compiler 	( Compiler(..), CompilerFlavor(..) )
import Distribution.PreProcess 	( ppCpp, PreProcessor, runSimplePreProcessor )
import Distribution.PreProcess.Unlit
				( unlit )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), autogenModulesDir )
import Distribution.Simple.Utils( rawSystemExit, die,
				  dirOf, dotToSep, moduleToFilePath,
				  smartCopySources, findFile )
import Language.Haskell.Extension
				( Extension(..) )
import Distribution.Compat.Directory
				( copyFile,createDirectoryIfMissing,
				  removeDirectoryRecursive )
import Distribution.Compat.FilePath
				( joinFileName, splitFileExt, joinFileExt,
                                  dllExtension, searchPathSeparator,
                                  platformPath )
import Distribution.Verbosity

import Data.Char		( isSpace )
import Data.Maybe		( mapMaybe )
import Control.Monad		( unless, when, filterM )
#ifndef __NHC__
import Control.Exception	( try )
#else
import IO			( try )
#endif
import Data.List		( nub, sort, isSuffixOf )
import System.Directory		( Permissions(..), getPermissions,
				  setPermissions )

-- -----------------------------------------------------------------------------

-- |Building a package for Hugs.
build :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
build pkg_descr lbi verbosity = do
    let pref = scratchDir lbi
    createDirectoryIfMissing True pref
    withLib pkg_descr () $ \ l -> do
	copyFile (autogenModulesDir lbi `joinFileName` paths_modulename)
		(pref `joinFileName` paths_modulename)
	compileBuildInfo pref [] (libModules pkg_descr) (libBuildInfo l)
    withExe pkg_descr $ compileExecutable (pref `joinFileName` "programs")
  where
	srcDir = buildDir lbi

	paths_modulename = autogenModuleName pkg_descr ++ ".hs"

	compileExecutable :: FilePath -> Executable -> IO ()
	compileExecutable destDir (exe@Executable {modulePath=mainPath, buildInfo=bi}) = do
            let exeMods = otherModules bi
	    srcMainFile <- findFile (hsSourceDirs bi) mainPath
	    let exeDir = destDir `joinFileName` exeName exe
	    let destMainFile = exeDir `joinFileName` hugsMainFilename exe
	    copyModule (CPP `elem` extensions bi) bi srcMainFile destMainFile
	    let destPathsFile = exeDir `joinFileName` paths_modulename
	    copyFile (autogenModulesDir lbi `joinFileName` paths_modulename)
		     destPathsFile
	    compileBuildInfo exeDir (maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr)) exeMods bi
	    compileFiles bi exeDir [destMainFile, destPathsFile]
	
	compileBuildInfo :: FilePath -- ^output directory
                         -> [FilePath] -- ^library source dirs, if building exes
                         -> [String] -- ^Modules
                         -> BuildInfo -> IO ()
	compileBuildInfo destDir mLibSrcDirs mods bi = do
	    -- Pass 1: copy or cpp files from build directory to scratch directory
	    let useCpp = CPP `elem` extensions bi
	    let srcDirs = nub $ srcDir : hsSourceDirs bi ++ mLibSrcDirs
            when (verbosity >= verbose)
                 (putStrLn $ "Source directories: " ++ show srcDirs)
            flip mapM_ mods $ \ m -> do
                fs <- moduleToFilePath srcDirs m suffixes
                case fs of
                  [] ->
                    die ("can't find source for module " ++ m)
                  srcFile:_ -> do
                    let (_, ext) = splitFileExt srcFile
                    copyModule useCpp bi srcFile
                        (destDir `joinFileName` dotToSep m `joinFileExt` ext)
	    -- Pass 2: compile foreign stubs in scratch directory
	    stubsFileLists <- sequence [moduleToFilePath [destDir] modu suffixes |
			modu <- mods]
            compileFiles bi destDir (concat stubsFileLists)

	suffixes = ["hs", "lhs"]

	-- Copy or cpp a file from the source directory to the build directory.
	copyModule :: Bool -> BuildInfo -> FilePath -> FilePath -> IO ()
	copyModule cppAll bi srcFile destFile = do
	    createDirectoryIfMissing True (dirOf destFile)
	    (exts, opts, _) <- getOptionsFromSource srcFile
	    let ghcOpts = hcOptions GHC opts
	    if cppAll || CPP `elem` exts || "-cpp" `elem` ghcOpts then do
	    	runSimplePreProcessor (ppCpp bi lbi) srcFile destFile verbosity
	    	return ()
	      else
	    	copyFile srcFile destFile

        compileFiles :: BuildInfo -> FilePath -> [FilePath] -> IO ()
        compileFiles bi modDir fileList = do
	    ffiFileList <- filterM testFFI fileList
            unless (null ffiFileList) $ do
                when (verbosity >= normal) (putStrLn "Compiling FFI stubs")
                mapM_ (compileFFI bi modDir) ffiFileList

        -- Only compile FFI stubs for a file if it contains some FFI stuff
        testFFI :: FilePath -> IO Bool
        testFFI file = do
            inp <- readHaskellFile file
            return ("foreign" `elem` symbols (stripComments False inp))

        compileFFI :: BuildInfo -> FilePath -> FilePath -> IO ()
        compileFFI bi modDir file = do
            (_, opts, file_incs) <- getOptionsFromSource file
            let ghcOpts = hcOptions GHC opts
            let pkg_incs = ["\"" ++ inc ++ "\"" 
			   | inc <- includes bi ++ installIncludes bi]
            let incs = nub (sort (file_incs ++ includeOpts ghcOpts ++ pkg_incs))
            let pathFlag = "-P" ++ modDir ++ [searchPathSeparator]
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
            rawSystemExit verbosity ffihugs (hugsArgs ++ file : cArgs)

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
	stripCommentsLevel n (_:cs) = stripCommentsLevel n cs
	stripCommentsLevel _ [] = []

	copyString ('\\':c:cs) = '\\' : c : copyString cs
	copyString ('"':cs) = '"' : stripCommentsLevel 0 cs
	copyString (c:cs) = c : copyString cs
	copyString [] = []

	copyPragma ('#':'-':'}':cs) = '#' : '-' : '}' : stripCommentsLevel 0 cs
	copyPragma (c:cs) = c : copyPragma cs
	copyPragma [] = []

-- -----------------------------------------------------------------------------
-- |Install for Hugs.
-- For install, copy-prefix = prefix, but for copy they're different.
-- The library goes in \<copy-prefix>\/lib\/hugs\/packages\/\<pkgname>
-- (i.e. \<prefix>\/lib\/hugs\/packages\/\<pkgname> on the target system).
-- Each executable goes in \<copy-prefix>\/lib\/hugs\/programs\/\<exename>
-- (i.e. \<prefix>\/lib\/hugs\/programs\/\<exename> on the target system)
-- with a script \<copy-prefix>\/bin\/\<exename> pointing at
-- \<prefix>\/lib\/hugs\/programs\/\<exename>.
install
    :: Verbosity -- ^verbosity
    -> FilePath  -- ^Library install location
    -> FilePath  -- ^Program install location
    -> FilePath  -- ^Executable install location
    -> FilePath  -- ^Program location on target system
    -> FilePath  -- ^Build location
    -> PackageDescription
    -> IO ()
install verbosity libDir installProgDir binDir targetProgDir buildPref pkg_descr = do
    try $ removeDirectoryRecursive libDir
    smartCopySources verbosity [buildPref] libDir (libModules pkg_descr) hugsInstallSuffixes True False
    let buildProgDir = buildPref `joinFileName` "programs"
    when (any (buildable . buildInfo) (executables pkg_descr)) $
        createDirectoryIfMissing True binDir
    withExe pkg_descr $ \ exe -> do
        let theBuildDir = buildProgDir `joinFileName` exeName exe
        let installDir = installProgDir `joinFileName` exeName exe
        let targetDir = targetProgDir `joinFileName` exeName exe
        try $ removeDirectoryRecursive installDir
        smartCopySources verbosity [theBuildDir] installDir
            ("Main" : autogenModuleName pkg_descr : otherModules (buildInfo exe)) hugsInstallSuffixes True False
        let targetName = "\"" ++ (targetDir `joinFileName` hugsMainFilename exe) ++ "\""
        -- FIX (HUGS): use extensions, and options from file too?
        -- see http://hackage.haskell.org/trac/hackage/ticket/43
        let hugsOptions = hcOptions Hugs (options (buildInfo exe))
#if mingw32_HOST_OS || mingw32_TARGET_OS
        let exeFile = binDir `joinFileName` exeName exe `joinFileExt` "bat"
        let script = unlines [
                "@echo off",
                unwords ("runhugs" : hugsOptions ++ [targetName, "%*"])]
#else
        let exeFile = binDir `joinFileName` exeName exe
        let script = unlines [
                "#! /bin/sh",
                unwords ("runhugs" : hugsOptions ++ [targetName, "\"$@\""])]
#endif
        writeFile exeFile script
        perms <- getPermissions exeFile
        setPermissions exeFile perms { executable = True, readable = True }

hugsInstallSuffixes :: [String]
hugsInstallSuffixes = ["hs", "lhs", dllExtension]

-- |Filename used by Hugs for the main module of an executable.
-- This is a simple filename, so that Hugs will look for any auxiliary
-- modules it uses relative to the directory it's in.
hugsMainFilename :: Executable -> FilePath
hugsMainFilename exe = "Main" `joinFileExt` ext
  where (_, ext) = splitFileExt (modulePath exe)
