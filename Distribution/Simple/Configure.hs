{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
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

module Distribution.Simple.Configure (writePersistBuildConfig,
                                      getPersistBuildConfig,
                                      LocalBuildInfo(..),
 			  	      configure)
    where

import Distribution.Setup(ConfigFlags,CompilerFlavor(..), Compiler(..))
import Distribution.Package(PackageDescription(..))
import Distribution.Simple.Utils
import Distribution.Package	( PackageIdentifier )

import System.IO hiding (catch)
import System.Exit
import System.Directory
import Control.Monad		( when )
import Control.Exception	( catch, evaluate )
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
	packageDeps :: [PackageIdentifier]
		-- ^ Which packages we depend on, *exactly*,  The
		-- 'PackageDescription' specifies a set of build dependencies
		-- that must be satisfied in terms of version ranges.  This
		-- field fixes those dependencies to the specific versions
		-- available on this machine for this compiler.
  }
  deriving (Show, Read, Eq)

emptyLocalBuildInfo :: LocalBuildInfo
emptyLocalBuildInfo = undefined

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  str <- readFile localBuildInfoFile
  let bi = read str
  evaluate bi `catch` \_ -> 
	die "error reading .setup-config; run ./Setup.lhs configure?\n"
  return bi

writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = do
  writeFile localBuildInfoFile (show lbi)

localBuildInfoFile :: FilePath
localBuildInfoFile = "./.setup-config"

-- -----------------------------------------------------------------------------
-- Configuration

configure :: PackageDescription -> ConfigFlags -> IO LocalBuildInfo
configure pkg_descr (maybe_hc_flavor, maybe_hc_path, maybe_prefix)
  = do
	setupMessage "Configuring" pkg_descr
	-- prefix
	let prefix = case maybe_prefix of
			Just path -> path
			Nothing   -> system_default_prefix pkg_descr

        message $ "Using build prefix: " ++ prefix
	-- detect compiler
	compiler@(Compiler f' p' pkg) <- configCompiler maybe_hc_flavor maybe_hc_path pkg_descr 
        message $ "Using compiler flavor: " ++ (show f')
        message $ "Using compiler: " ++ p'
        message $ "Using package tool: " ++ pkg
	return LocalBuildInfo{prefix=prefix, compiler=compiler, packageDeps=[]}

system_default_prefix :: PackageDescription -> String
system_default_prefix PackageDescription{package=package} = 
#ifdef mingw32_TARGET_OS
  "C:\Program Files\" ++ pkgName package
#else
  "/usr"
#endif

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> PackageDescription
  -> IO Compiler

configCompiler (Just flavor) (Just path) _
  = do pkgtool <- guessPkgToolFromHCPath flavor path
       return (Compiler{compilerFlavor=flavor,
			compilerPath=path,
			compilerPkgTool=pkgtool})

configCompiler (Just flavor) Nothing _
  = do path <- findCompiler flavor
       pkgtool <- guessPkgToolFromHCPath flavor path
       return (Compiler{compilerFlavor=flavor,
			compilerPath=path,
			compilerPkgTool=pkgtool})

configCompiler Nothing maybe_path pkg_descr
  = configCompiler (Just defaultCompilerFlavor) maybe_path pkg_descr

defaultCompilerFlavor =
#if defined(__GLASGOW_HASKELL__)
   GHC
#elif defined(__NHC__)
   NHC
#elif defined(__HUGS__)
   Hugs
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
  = do let pkgToolName = compilerPkgToolName flavor
           (dir,_) = splitFilenameDir path
           pkgtool = dir ++ '/':pkgToolName
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
hunitTests :: IO [Test]
hunitTests = do
   let simonMarGHCLoc = "/home/simonmar/fp/bin/i386-unknown-linux/ghc"
   simonMarGHC <- configure PackageDescription{} (Just GHC,
				       Just simonMarGHCLoc,
				       Nothing)
   return $ [TestLabel "Configure Testing" $ TestList [
	     "finding ghc, etc on simonMar's machine" ~: "failed" ~:
             (LocalBuildInfo "/usr" (Compiler GHC 
	                    simonMarGHCLoc
 			    (simonMarGHCLoc ++ "-pkg")) [])
             ~=? simonMarGHC]]
#endif
