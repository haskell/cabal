{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Perform the \"@.\/setup configure@\" action.
-- Outputs the @.setup-config@ file.

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

module Distribution.Simple.Configure (writePersistBuildConfig,
                                      getPersistBuildConfig,
                                      LocalBuildInfo(..),
 			  	      configure,
                                      localBuildInfoFile,
                                      exeDeps,
				      buildDepToDep,
                                      findProgram,
#ifdef DEBUG
                                      hunitTests
#endif
                                     )
    where

#if __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Register (removeInstalledConfig)
import Distribution.Extension(extensionsToGHCFlag,
                         extensionsToNHCFlag, extensionsToHugsFlag)
import Distribution.Setup(ConfigFlags(..),CompilerFlavor(..), Compiler(..))
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription(
 	PackageDescription(..), Library(..),
	BuildInfo(..), Executable(..), setupMessage)
import Distribution.Simple.Utils (die, withTempFile,maybeExit)
import Distribution.Version (Version(..), VersionRange(..), Dependency(..),
			     parseVersion, showVersion)

import Data.List (intersperse, nub)
import Data.Char (isSpace)
import Data.Maybe(fromMaybe)
import System.Directory
import Distribution.Compat.FilePath (splitFilePath, joinFileName, joinFileExt)
import System.Cmd		( system )
import Control.Monad		( when, unless )
import Distribution.Compat.ReadP
import Distribution.Compat.Directory (findExecutable)
#ifndef __NHC__
import Control.Exception	( catch, evaluate )
#endif
import Data.Char (isDigit)
import Prelude hiding (catch)
#ifdef mingw32_TARGET_OS
import Foreign
import Foreign.C
#endif


#ifdef DEBUG
import HUnit
#endif

-- |Throws an error if it's not found.
exeDeps :: String -> LocalBuildInfo -> [PackageIdentifier]
exeDeps s d = fromMaybe (error "Incorrect executableDeps in .setup-config file.  Re-run configure?") (lookup s (executableDeps d))

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  e <- doesFileExist localBuildInfoFile
  let dieMsg = "error reading " ++ localBuildInfoFile ++ "; run \"setup configure\" command?\n"
  when (not e) (die dieMsg)
  str <- readFile localBuildInfoFile
  let bi = read str
#ifndef __NHC__
  evaluate bi `catch` \_ -> 
	die dieMsg
#else
-- FIXME: Is there anything we can do here? DeepSeq?
#endif
  return bi

writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = do
  writeFile localBuildInfoFile (show lbi)

localBuildInfoFile :: FilePath
localBuildInfoFile = "./.setup-config"

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

configure :: PackageDescription -> ConfigFlags -> IO LocalBuildInfo
configure pkg_descr cfg
  = do
	setupMessage "Configuring" pkg_descr
	removeInstalledConfig
        let lib = library pkg_descr
	-- prefix
	defPrefix <- system_default_prefix pkg_descr
        let pref = fromMaybe defPrefix (configPrefix cfg)
	-- detect compiler
	comp@(Compiler f' ver p' pkg) <- configCompiler (configHcFlavor cfg) (configHcPath cfg) (configHcPkg cfg) pkg_descr
        -- check extensions
        let extlist = nub $ maybe [] (extensions . libBuildInfo) lib ++
                      concat [ extensions exeBi | Executable _ _ exeBi <- executables pkg_descr ]
        let exts = case f' of
                     GHC  -> fst $ extensionsToGHCFlag extlist
                     NHC  -> fst $ extensionsToNHCFlag extlist
                     Hugs -> fst $ extensionsToHugsFlag extlist
                     _    -> [] -- Hmm.
        unless (null exts) $ putStrLn $ -- Just warn, FIXME: Should this be an error?
            "Warning: " ++ show f' ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))
        haddock <- findProgram "haddock" (configHaddock cfg)
        happy   <- findProgram "happy"   (configHappy cfg)
        alex    <- findProgram "alex"    (configAlex cfg)
        hsc2hs  <- findProgram "hsc2hs"  (configHsc2hs cfg)
        cpphs   <- findProgram "cpphs"   (configCpphs cfg)
        -- FIXME: maybe this should only be printed when verbose?
        message $ "Using install prefix: " ++ pref
        message $ "Using compiler: " ++ p'
        message $ "Compiler flavor: " ++ (show f')
        message $ "Compiler version: " ++ showVersion ver
        message $ "Using package tool: " ++ pkg
        reportProgram "haddock" haddock
        reportProgram "happy"   happy
        reportProgram "alex"    alex
        reportProgram "hsc2hs"  hsc2hs
        reportProgram "cpphs"   cpphs
	return LocalBuildInfo{prefix=pref, compiler=comp,
			      buildDir="dist" `joinFileName` "build",
                              packageDeps=map buildDepToDep (buildDepends pkg_descr),
                              withHaddock=haddock,
                              withHappy=happy, withAlex=alex,
                              withHsc2hs=hsc2hs, withCpphs=cpphs,
                              executableDeps = [(n, map buildDepToDep (buildDepends pkg_descr))
                                                | Executable n _ _ <- executables pkg_descr]
                             }

-- |Return the explicit path if given, otherwise look for the program
-- name in the path.
findProgram
    :: String              -- ^ program name
    -> Maybe FilePath      -- ^ optional explicit path
    -> IO (Maybe FilePath)
findProgram name Nothing = findExecutable name
findProgram _ p = return p

reportProgram :: String -> Maybe FilePath -> IO ()
reportProgram name Nothing = message ("No " ++ name ++ " found")
reportProgram name (Just p) = message ("Using " ++ name ++ ": " ++ p)

-- |Converts build dependencies to real dependencies.  FIX: doesn't
-- set any version information - will need to query HC-PKG for this.
buildDepToDep :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
buildDepToDep (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, calculate it from the installed module. FIX: not
-- implemented because HC-PKG doesn't yet do this.
buildDepToDep (Dependency s _) = PackageIdentifier s (Version [] [])

system_default_prefix :: PackageDescription -> IO String
#ifdef mingw32_TARGET_OS
system_default_prefix PackageDescription{package=pkg} =
  allocaBytes long_path_size $ \pPath -> do
     r <- c_SHGetFolderPath nullPtr csidl_PROGRAM_FILES nullPtr 0 pPath
     s <- peekCString pPath
     return (s++'\\':pkgName pkg)
  where
    csidl_PROGRAM_FILES = 0x0026
    long_path_size      = 1024

foreign import stdcall unsafe "SHGetFolderPath" 
            c_SHGetFolderPath :: Ptr () 
                              -> CInt 
                              -> Ptr () 
                              -> CInt 
                              -> CString 
                              -> IO CInt
#else
system_default_prefix _ = 
  return "/usr/local"
#endif

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
  -> PackageDescription -> IO Compiler

configCompiler (Just flavor) maybe_compiler maybe_pkgtool _
  = do comp <- 
	 case maybe_compiler of
	   Just path -> return path
	   Nothing   -> findCompiler flavor

       ver <- configCompilerVersion flavor comp

       pkgtool <-
	 case maybe_pkgtool of
	   Just path -> return path
	   Nothing   -> guessPkgToolFromHCPath flavor comp

       return (Compiler{compilerFlavor=flavor,
			compilerVersion=ver,
			compilerPath=comp,
			compilerPkgTool=pkgtool})

configCompiler Nothing maybe_path maybe_hc_pkg pkg_descr
  = configCompiler (Just defaultCompilerFlavor) 
	maybe_path maybe_hc_pkg pkg_descr

defaultCompilerFlavor :: CompilerFlavor
defaultCompilerFlavor =
#if defined(__GLASGOW_HASKELL__)
   GHC
#elif defined(__NHC__)
   NHC
#elif defined(__HUGS__)
   Hugs
#else
   error "Unknown compiler"
#endif

findCompiler :: CompilerFlavor -> IO FilePath
findCompiler flavor = do
  let prog = compilerBinaryName flavor
  message $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
   Nothing   -> die ("Cannot find compiler for " ++ prog)
   Just path -> do message ("found " ++ prog ++ " at "++ path)
		   return path
   -- ToDo: check that compiler works? check compiler version?

compilerBinaryName :: CompilerFlavor -> String
compilerBinaryName GHC  = "ghc"
compilerBinaryName NHC  = "hmake" -- FIX: uses hmake for now
compilerBinaryName Hugs = "ffihugs"

compilerPkgToolName :: CompilerFlavor -> String
compilerPkgToolName GHC  = "ghc-pkg"
compilerPkgToolName NHC  = "hmake" -- FIX: nhc98-pkg Does not yet exist
compilerPkgToolName Hugs = "hugs" -- FIX (HUGS): hugs-pkg does not yet exist

configCompilerVersion :: CompilerFlavor -> FilePath -> IO Version
configCompilerVersion GHC compiler =
  withTempFile "." "" $ \tmp -> do
    maybeExit $ system (compiler ++ " --version >" ++ tmp)
    str <- readFile tmp
    case pCheck (readP_to_S parseVersion (dropWhile (not.isDigit) str)) of
	[v] -> return v
	_   -> die ("cannot determine version of " ++ compiler ++ ":\n  "
			++ str)
configCompilerVersion _ _ = return Version{ versionBranch=[],versionTags=[] }

pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

guessPkgToolFromHCPath :: CompilerFlavor -> FilePath -> IO FilePath
guessPkgToolFromHCPath flavor path
  = do let pkgToolName     = compilerPkgToolName flavor
           (dir,_,ext) = splitFilePath path
           pkgtool         = dir `joinFileName` pkgToolName `joinFileExt` ext
       message $ "looking for package tool: " ++ pkgToolName ++ " near compiler in " ++ path
       exists <- doesFileExist pkgtool
       when (not exists) $
	  die ("Cannot find package tool: " ++ pkgtool)
       message $ "found package tool in " ++ pkgtool
       return pkgtool

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s

-- -----------------------------------------------------------------------------
-- Tests

#ifdef DEBUG
packageID = PackageIdentifier "Foo" (Version [1] [])

hunitTests :: [Test]
hunitTests = []
{- Too specific:
    = [TestCase $
       do let simonMarGHCLoc = "/usr/bin/ghc"
          simonMarGHC <- configure emptyPackageDescription {package=packageID}
                                       (Just GHC,
				       Just simonMarGHCLoc,
				       Nothing, Nothing)
	  assertEqual "finding ghc, etc on simonMar's machine failed"
             (LocalBuildInfo "/usr" (Compiler GHC 
	                    (Version [6,2,2] []) simonMarGHCLoc 
 			    (simonMarGHCLoc ++ "-pkg")) [] [])
             simonMarGHC
      ]
-}
#endif
