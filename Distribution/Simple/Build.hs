-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Invokes the "Distribution.Compiler"s to build the library and
-- executables in this package.

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
	build, makefile, initialBuildSteps
  ) where

import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor )
import Distribution.PackageDescription 
				( PackageDescription(..), BuildInfo(..),
				  Executable(..), Library(..) )
import Distribution.Package
         ( packageVersion, showPackageId, Package(..) )
import Distribution.Simple.Setup ( CopyDest(..), BuildFlags(..),
                                  MakefileFlags(..), fromFlag )
import Distribution.Simple.PreProcess  ( preprocessSources, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..),
                                  InstallDirs(..), absoluteInstallDirs,
                                  prefixRelativeInstallDirs )
import Distribution.Simple.BuildPaths ( autogenModuleName )
import Distribution.Simple.Configure
				( localBuildInfoFile )
import Distribution.Simple.Utils
        ( createDirectoryIfMissingVerbose, die, setupMessage, writeUTF8File )
import Distribution.System

import System.FilePath          ( (</>), pathSeparator )

import Data.Maybe		( maybeToList, fromJust, isNothing )
import Control.Monad 		( unless, when )
import System.Directory		( getModificationTime, doesFileExist )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Distribution.PackageDescription (hasLibs)
import Distribution.Verbosity

-- -----------------------------------------------------------------------------
-- |Build the libraries and executables in this package.

build    :: PackageDescription  -- ^mostly information from the .cabal file
         -> LocalBuildInfo -- ^Configuration information
         -> BuildFlags -- ^Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  let verbosity = fromFlag (buildVerbose flags)
  initialBuildSteps pkg_descr lbi verbosity suffixes
  setupMessage verbosity "Building" (packageId pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.build  pkg_descr lbi verbosity
    JHC  -> JHC.build  pkg_descr lbi verbosity
    Hugs -> Hugs.build pkg_descr lbi verbosity
    NHC  -> NHC.build  pkg_descr lbi verbosity
    _    -> die ("Building is not supported with this compiler.")

makefile :: PackageDescription  -- ^mostly information from the .cabal file
         -> LocalBuildInfo -- ^Configuration information
         -> MakefileFlags -- ^Flags that the user passed to makefile
         -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
         -> IO ()
makefile pkg_descr lbi flags suffixes = do
  let verbosity = fromFlag (makefileVerbose flags)
  initialBuildSteps pkg_descr lbi verbosity suffixes
  when (not (hasLibs pkg_descr)) $
      die ("Makefile is only supported for libraries, currently.")
  setupMessage verbosity "Generating Makefile" (packageId pkg_descr)
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.makefile  pkg_descr lbi flags
    _    -> die ("Generating a Makefile is not supported for this compiler.")


initialBuildSteps :: PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> Verbosity -- ^The verbosity to use
                  -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
                  -> IO ()
initialBuildSteps pkg_descr lbi verbosity suffixes = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = showPackageId (packageId pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)

  -- construct and write the Paths_<pkg>.hs file
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)
  buildPathsModule pkg_descr lbi

  preprocessSources pkg_descr lbi False verbosity suffixes

-- ------------------------------------------------------------
-- * Building Paths_<pkg>.hs
-- ------------------------------------------------------------

-- The directory in which we put auto-generated modules
autogenModulesDir :: LocalBuildInfo -> String
autogenModulesDir lbi = buildDir lbi </> "autogen"

buildPathsModule :: PackageDescription -> LocalBuildInfo -> IO ()
buildPathsModule pkg_descr lbi =
   let pragmas
	| absolute || isHugs = ""
	| otherwise =
          "{-# LANGUAGE ForeignFunctionInterface #-}\n" ++
          "{-# OPTIONS_GHC -fffi #-}\n"++
          "{-# OPTIONS_JHC -fffi #-}\n"

       foreign_imports
	| absolute = ""
	| isHugs = "import System.Environment\n"
	| otherwise =
	  "import Foreign\n"++
	  "import Foreign.C\n"++
	  "import Data.Maybe\n"

       header =
	pragmas++
	"module " ++ paths_modulename ++ " (\n"++
	"\tversion,\n"++
	"\tgetBinDir, getLibDir, getDataDir, getLibexecDir,\n"++
	"\tgetDataFileName\n"++
	"\t) where\n"++
	"\n"++
	foreign_imports++
	"import Data.Version"++
	"\n"++
	"\nversion :: Version"++
	"\nversion = " ++ show (packageVersion pkg_descr)++
	"\n"

       body
	| absolute =
	  "\nbindir, libdir, datadir, libexecdir :: FilePath\n"++
	  "\nbindir     = " ++ show flat_bindir ++
	  "\nlibdir     = " ++ show flat_libdir ++
	  "\ndatadir    = " ++ show flat_datadir ++
	  "\nlibexecdir = " ++ show flat_libexecdir ++
	  "\n"++
	  "\ngetBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath\n"++
	  "getBinDir = return bindir\n"++
	  "getLibDir = return libdir\n"++
	  "getDataDir = return datadir\n"++
	  "getLibexecDir = return libexecdir\n" ++
	  "\n"++
	  "getDataFileName :: FilePath -> IO FilePath\n"++
	  "getDataFileName name = return (datadir ++ "++path_sep++" ++ name)\n"
	| otherwise =
	  "\nprefix        = " ++ show flat_prefix ++
	  "\nbindirrel     = " ++ show (fromJust flat_bindirrel) ++
	  "\n\n"++
	  "getBinDir :: IO FilePath\n"++
	  "getBinDir = getPrefixDirRel bindirrel\n\n"++
	  "getLibDir :: IO FilePath\n"++
	  "getLibDir = "++mkGetDir flat_libdir flat_libdirrel++"\n\n"++
	  "getDataDir :: IO FilePath\n"++
	  "getDataDir =  "++mkGetDir flat_datadir flat_datadirrel++"\n\n"++
	  "getLibexecDir :: IO FilePath\n"++
	  "getLibexecDir = "++mkGetDir flat_libexecdir flat_libexecdirrel++"\n\n"++
	  "getDataFileName :: FilePath -> IO FilePath\n"++
	  "getDataFileName name = do\n"++
	  "  dir <- getDataDir\n"++
	  "  return (dir `joinFileName` name)\n"++
	  "\n"++
	  get_prefix_stuff++
	  "\n"++
	  filename_stuff
   in do btime <- getModificationTime localBuildInfoFile
   	 exists <- doesFileExist paths_filepath
   	 ptime <- if exists
   	            then getModificationTime paths_filepath
   	            else return btime
	 if btime >= ptime
	   then writeUTF8File paths_filepath (header++body)
	   else return ()
 where
	InstallDirs {
          prefix     = flat_prefix,
          bindir     = flat_bindir,
          libdir     = flat_libdir,
          datadir    = flat_datadir,
          libexecdir = flat_libexecdir
        } = absoluteInstallDirs pkg_descr lbi NoCopyDest
        InstallDirs {
          bindir     = flat_bindirrel,
          libdir     = flat_libdirrel,
          datadir    = flat_datadirrel,
          libexecdir = flat_libexecdirrel,
          progdir    = flat_progdirrel
        } = prefixRelativeInstallDirs pkg_descr lbi
	
	mkGetDir _   (Just dirrel) = "getPrefixDirRel " ++ show dirrel
	mkGetDir dir Nothing       = "return " ++ show dir

        -- In several cases we cannot make relocatable installations
        absolute =
             hasLibs pkg_descr        -- we can only make progs relocatable
          || isNothing flat_bindirrel -- if the bin dir is an absolute path
          || not (supportsRelocatableProgs (compilerFlavor (compiler lbi)))

        supportsRelocatableProgs Hugs = True
        supportsRelocatableProgs GHC  = case buildOS of
                           Windows   -> True
                           _         -> False
        supportsRelocatableProgs _    = False

  	paths_modulename = autogenModuleName pkg_descr
	paths_filename = paths_modulename ++ ".hs"
	paths_filepath = autogenModulesDir lbi </> paths_filename

	isHugs = compilerFlavor (compiler lbi) == Hugs
        get_prefix_stuff
          | isHugs    = "progdirrel :: String\n"++
                        "progdirrel = "++show (fromJust flat_progdirrel)++"\n\n"++
                        get_prefix_hugs
          | otherwise = get_prefix_win32

	path_sep = show [pathSeparator]

get_prefix_win32 :: String
get_prefix_win32 =
  "getPrefixDirRel :: FilePath -> IO FilePath\n"++
  "getPrefixDirRel dirRel = do \n"++
  "  let len = (2048::Int) -- plenty, PATH_MAX is 512 under Win32.\n"++
  "  buf <- mallocArray len\n"++
  "  ret <- getModuleFileName nullPtr buf len\n"++
  "  if ret == 0 \n"++
  "     then do free buf;\n"++
  "             return (prefix `joinFileName` dirRel)\n"++
  "     else do exePath <- peekCString buf\n"++
  "             free buf\n"++
  "             let (bindir,_) = splitFileName exePath\n"++
  "             return ((bindir `minusFileName` bindirrel) `joinFileName` dirRel)\n"++
  "\n"++
  "foreign import stdcall unsafe \"windows.h GetModuleFileNameA\"\n"++
  "  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32\n"

get_prefix_hugs :: String
get_prefix_hugs =
  "getPrefixDirRel :: FilePath -> IO FilePath\n"++
  "getPrefixDirRel dirRel = do\n"++
  "  mainPath <- getProgName\n"++
  "  let (progPath,_) = splitFileName mainPath\n"++
  "  let (progdir,_) = splitFileName progPath\n"++
  "  return ((progdir `minusFileName` progdirrel) `joinFileName` dirRel)\n"

filename_stuff :: String
filename_stuff =
  "minusFileName :: FilePath -> String -> FilePath\n"++
  "minusFileName dir \"\"     = dir\n"++
  "minusFileName dir \".\"    = dir\n"++
  "minusFileName dir suffix =\n"++
  "  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))\n"++
  "\n"++
  "joinFileName :: String -> String -> FilePath\n"++
  "joinFileName \"\"  fname = fname\n"++
  "joinFileName \".\" fname = fname\n"++
  "joinFileName dir \"\"    = dir\n"++
  "joinFileName dir fname\n"++
  "  | isPathSeparator (last dir) = dir++fname\n"++
  "  | otherwise                  = dir++pathSeparator:fname\n"++
  "\n"++
  "splitFileName :: FilePath -> (String, String)\n"++
  "splitFileName p = (reverse (path2++drive), reverse fname)\n"++
  "  where\n"++
  "    (path,drive) = case p of\n"++
  "       (c:':':p) -> (reverse p,[':',c])\n"++
  "       _         -> (reverse p,\"\")\n"++
  "    (fname,path1) = break isPathSeparator path\n"++
  "    path2 = case path1 of\n"++
  "      []                           -> \".\"\n"++
  "      [_]                          -> path1   -- don't remove the trailing slash if \n"++
  "                                              -- there is only one character\n"++
  "      (c:path) | isPathSeparator c -> path\n"++
  "      _                            -> path1\n"++
  "\n"++
  "pathSeparator :: Char\n"++
  (case buildOS of
       Windows   -> "pathSeparator = '\\\\'\n"
       _         -> "pathSeparator = '/'\n") ++
  "\n"++
  "isPathSeparator :: Char -> Bool\n"++
  (case buildOS of
       Windows   -> "isPathSeparator c = c == '/' || c == '\\\\'\n"
       _         -> "isPathSeparator c = c == '/'\n")
