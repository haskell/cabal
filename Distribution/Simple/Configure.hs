{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC
--
-- Explanation: Perform the ".\/setup configure" action.  Outputs the
-- .setup-config file.

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
#ifdef DEBUG
                                      hunitTests
#endif
                                     )
    where

#if __GLASGOW_HASKELL__ < 603 
#include "config.h"
#endif

import Distribution.Misc(Dependency(..), extensionsToGHCFlag,
                         extensionsToNHCFlag, extensionsToHugsFlag)
import Distribution.Setup(ConfigFlags,CompilerFlavor(..), Compiler(..))
import Distribution.Package(PackageDescription(..), PackageIdentifier(..),
                            BuildInfo(..), Executable(..)
                           )
import Distribution.Simple.Utils (die, setupMessage, findBinary, 
                                  splitFilePath, joinFilenameDir,  joinExt)
import Distribution.Package	( PackageIdentifier )
import Distribution.Version (Version(..), VersionRange(..))

import Data.List (intersperse, nub)
import Data.Maybe(fromJust)
import System.Directory
import Control.Monad		( when, unless )
#ifndef __NHC__
import Control.Exception	( catch, evaluate )
#endif
import Prelude hiding (catch)

#ifdef DEBUG
import HUnit
#endif

-- |Data cached after configuration step.
data LocalBuildInfo = LocalBuildInfo {
  	prefix	    :: FilePath,
		-- ^ The installation directory (eg. @/usr/local@, or
		-- @C:/Program Files/foo-1.2@ on Windows.
	compiler    :: Compiler,
		-- ^ The compiler we're building with
	packageDeps :: [PackageIdentifier],
		-- ^ Which packages we depend on, *exactly*,  The
		-- 'PackageDescription' specifies a set of build dependencies
		-- that must be satisfied in terms of version ranges.  This
		-- field fixes those dependencies to the specific versions
		-- available on this machine for this compiler.
        executableDeps :: [(String,[PackageIdentifier])]
  }
  deriving (Show, Read, Eq)

-- |Throws an error if it's not found.
exeDeps :: String -> LocalBuildInfo -> [PackageIdentifier]
exeDeps s d = fromJust $ lookup s (executableDeps d)

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  str <- readFile localBuildInfoFile
  let bi = read str
#ifndef __NHC__
  evaluate bi `catch` \_ -> 
	die "error reading .setup-config; run ./Setup.lhs configure?\n"
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
configure pkg_descr (maybe_hc_flavor, maybe_hc_path, maybe_hc_pkg, maybe_prefix)
  = do
	setupMessage "Configuring" pkg_descr
        let lib = library pkg_descr
	-- prefix
	let pref = case maybe_prefix of
			Just path -> path
			Nothing   -> system_default_prefix pkg_descr
	-- detect compiler
	comp@(Compiler f' p' pkg) <- configCompiler maybe_hc_flavor maybe_hc_path maybe_hc_pkg pkg_descr 
        -- check extensions
        let extlist = nub $ maybe [] extensions lib ++
                      concat [ extensions exeBi | Executable _ _ exeBi <- executables pkg_descr ]
        let exts = case f' of
                     GHC  -> fst $ extensionsToGHCFlag extlist
                     NHC  -> fst $ extensionsToNHCFlag extlist
                     Hugs -> fst $ extensionsToHugsFlag extlist
                     _    -> [] -- Hmm.
        unless (null exts) $ putStrLn $ -- Just warn, FIXME: Should this be an error?
            "Warning: " ++ show f' ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))

        -- FIXME: maybe this should only be printed when verbose?
        message $ "Using build prefix: " ++ pref
        message $ "Using compiler flavor: " ++ (show f')
        message $ "Using compiler: " ++ p'
        message $ "Using package tool: " ++ pkg
	return LocalBuildInfo{prefix=pref, compiler=comp,
                              packageDeps=map buildDepToDep (maybe [] buildDepends lib),
                              executableDeps = [(n, map buildDepToDep (buildDepends exeBi))
                                                | Executable n _ exeBi <- executables pkg_descr]
                             }

-- |Converts build dependencies to real dependencies.  FIX: doesn't
-- set any version information - will need to query HC-PKG for this.
buildDepToDep :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
buildDepToDep (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, calculate it from the installed module. FIX: not
-- implemented because HC-PKG doesn't yet do this.
buildDepToDep (Dependency s _) = PackageIdentifier s (Version [] [])

system_default_prefix :: PackageDescription -> String
#ifdef mingw32_TARGET_OS
system_default_prefix PackageDescription{package=pkg} = 
  "C:\\Program Files\\" ++ pkgName pkg
#else
system_default_prefix _ = 
  "/usr"
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

       pkgtool <-
	 case maybe_pkgtool of
	   Just path -> return path
	   Nothing   -> guessPkgToolFromHCPath flavor comp

       return (Compiler{compilerFlavor=flavor,
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
  res <- findBinary prog
  case res of
   Nothing   -> die ("Cannot find compiler for " ++ prog)
   Just path -> do message ("found " ++ prog ++ " at "++ path)
		   return path
   -- ToDo: check that compiler works? check compiler version?

compilerBinaryName :: CompilerFlavor -> String
compilerBinaryName GHC  = "ghc"
compilerBinaryName NHC  = "hmake" -- FIX: uses hmake for now
compilerBinaryName Hugs = "hugs"

compilerPkgToolName :: CompilerFlavor -> String
compilerPkgToolName GHC  = "ghc-pkg"
compilerPkgToolName NHC  = "hmake" -- FIX: nhc98-pkg Does not yet exist
compilerPkgToolName Hugs = "hugs-package"

guessPkgToolFromHCPath :: CompilerFlavor -> FilePath -> IO FilePath
guessPkgToolFromHCPath flavor path
  = do let pkgToolName     = compilerPkgToolName flavor
           (dir,_,ext) = splitFilePath path
           pkgtool         = dir `joinFilenameDir` pkgToolName `joinExt` ext
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
hunitTests
    = [TestCase $
       do let simonMarGHCLoc = "/usr/bin/ghc"
          simonMarGHC <- configure emptyPackageDescription {package=packageID}
                                       (Just GHC,
				       Just simonMarGHCLoc,
				       Nothing, Nothing)
	  assertEqual "finding ghc, etc on simonMar's machine failed"
             (LocalBuildInfo "/usr" (Compiler GHC 
	                    simonMarGHCLoc
 			    (simonMarGHCLoc ++ "-pkg")) [] [])
             simonMarGHC
      ]
#endif
