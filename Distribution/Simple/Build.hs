{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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

import Distribution.Compiler	( Compiler(..), CompilerFlavor(..) )
import Distribution.PackageDescription 
				( PackageDescription(..), BuildInfo(..),
				  setupMessage, Executable(..), Library(..), 
                                  autogenModuleName )
import Distribution.Package 	( PackageIdentifier(..), showPackageId )
import Distribution.Setup	 (CopyDest(..), BuildFlags(..) )
import Distribution.PreProcess  ( preprocessSources, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), mkBinDir, mkBinDirRel,
				  mkLibDir, mkLibDirRel, mkDataDir,mkDataDirRel,
				  mkLibexecDir, mkLibexecDirRel )
import Distribution.Simple.Configure
				( localBuildInfoFile )
import Distribution.Simple.Utils( die )

import Distribution.Compat.Directory
				( createDirectoryIfMissing )
import Distribution.Compat.FilePath
				( joinFileName, pathSeparator )

import Data.Maybe		( maybeToList, fromJust )
import Control.Monad 		( unless )
import System.Directory		( getModificationTime, doesFileExist)

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
-- import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

#ifdef mingw32_HOST_OS
import Distribution.PackageDescription (hasLibs)
#endif

#ifdef DEBUG
import HUnit (Test)
#endif

-- -----------------------------------------------------------------------------
-- Build the library

build :: PackageDescription
         -> LocalBuildInfo
         -> BuildFlags
         -> [ PPSuffixHandler ]
         -> IO ()
build pkg_descr lbi (BuildFlags verbose) suffixes = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = showPackageId (package pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissing True (buildDir lbi)

  -- construct and write the Paths_<pkg>.hs file
  createDirectoryIfMissing True (autogenModulesDir lbi)
  buildPathsModule pkg_descr lbi

  preprocessSources pkg_descr lbi verbose suffixes
  setupMessage "Building" pkg_descr
  case compilerFlavor (compiler lbi) of
   GHC  -> GHC.build  pkg_descr lbi verbose
   JHC  -> JHC.build  pkg_descr lbi verbose
   Hugs -> Hugs.build pkg_descr lbi verbose
   _    -> die ("Building is not supported with this compiler.")

-- ------------------------------------------------------------
-- * Building Paths_<pkg>.hs
-- ------------------------------------------------------------

-- The directory in which we put auto-generated modules
autogenModulesDir :: LocalBuildInfo -> String
autogenModulesDir lbi = buildDir lbi `joinFileName` "autogen"

buildPathsModule :: PackageDescription -> LocalBuildInfo -> IO ()
buildPathsModule pkg_descr lbi =
   let pragmas
	| absolute = ""
	| otherwise =
	  "{-# OPTIONS_GHC -fffi #-}\n"++
	  "{-# LANGUAGE ForeignFunctionInterface #-}\n"

       foreign_imports
	| absolute = ""
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
	"\nversion = " ++ show (pkgVersion (package pkg_descr))++
	"\n"

       body
	| absolute =
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
	  "\nprefix        = " ++ show (prefix lbi) ++
	  "\nbindirrel     = " ++ show (fromJust flat_bindirrel) ++
	  "\n"++
	  "\ngetBinDir :: IO FilePath\n"++
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
	  get_prefix_stuff
   in do btime <- getModificationTime localBuildInfoFile
   	 exists <- doesFileExist paths_filepath
   	 ptime <- if exists
   	            then getModificationTime paths_filepath
   	            else return btime
	 if btime >= ptime
	   then writeFile paths_filepath (header++body)
	   else return ()
 where
	flat_bindir        = mkBinDir pkg_descr lbi NoCopyDest
	flat_bindirrel     = mkBinDirRel pkg_descr lbi NoCopyDest
	flat_libdir        = mkLibDir pkg_descr lbi NoCopyDest
	flat_libdirrel     = mkLibDirRel pkg_descr lbi NoCopyDest
	flat_datadir       = mkDataDir pkg_descr lbi NoCopyDest
	flat_datadirrel    = mkDataDirRel pkg_descr lbi NoCopyDest
	flat_libexecdir    = mkLibexecDir pkg_descr lbi NoCopyDest
	flat_libexecdirrel = mkLibexecDirRel pkg_descr lbi NoCopyDest
	
	mkGetDir dir (Just dirrel) = "getPrefixDirRel " ++ show dirrel
	mkGetDir dir Nothing       = "return " ++ show dir

#if mingw32_HOST_OS
	absolute = hasLibs pkg_descr || flat_bindirrel == Nothing
#else
	absolute = True
#endif

  	paths_modulename = autogenModuleName pkg_descr
	paths_filename = paths_modulename ++ ".hs"
	paths_filepath = autogenModulesDir lbi `joinFileName` paths_filename

	path_sep = show [pathSeparator]

get_prefix_stuff :: String
get_prefix_stuff =
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
  "             return (prefixFromBinDir bindir bindirrel `joinFileName` dirRel)\n"++
  "  where\n"++
  "    prefixFromBinDir bindir path\n"++
  "      | path' == \".\" = bindir'\n"++
  "      | otherwise    = prefixFromBinDir bindir' path'\n"++
  "      where\n"++
  "        (bindir',_) = splitFileName bindir\n"++
  "        (path',  _) = splitFileName path\n"++
  "\n"++
  "foreign import stdcall unsafe \"windows.h GetModuleFileNameA\"\n"++
  "  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32\n"++
  "\n"++
  "joinFileName :: String -> String -> FilePath\n"++
  "joinFileName \"\"  fname = fname\n"++
  "joinFileName \".\" fname = fname\n"++
  "joinFileName dir \"\"    = dir\n"++
  "joinFileName dir fname\n"++
  "  | isPathSeparator (last dir) = dir++fname\n"++
  "  | otherwise                  = dir++pathSeparator:fname\n"++
  "\n"++
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
  "pathSeparator = '\\\\'\n"++
  "\n"++
  "isPathSeparator :: Char -> Bool\n"++
  "isPathSeparator ch =\n"++
  "  ch == '/' || ch == '\\\\'\n"

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
