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
import Distribution.Package(PackageConfig(..))
import Distribution.Simple.Utils (splitFilenameDir, die, split)

import System.IO
import System.Exit
import System.Directory
import System.Environment	( getEnv )
import Control.Monad		( when )

#ifdef DEBUG
import HUnit
#endif

-- |Data cached after configuration step.
data LocalBuildInfo = LocalBuildInfo {prefix :: String,
                                      compiler :: Compiler}
     deriving (Show, Read, Eq)

emptyLocalBuildInfo :: LocalBuildInfo
emptyLocalBuildInfo = undefined

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  str <- readFile localBuildInfoFile
  return (read str)

writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = do
  writeFile localBuildInfoFile (show lbi)

localBuildInfoFile :: FilePath
localBuildInfoFile = "./.setup-config"

-- -----------------------------------------------------------------------------
-- Configuration

configure :: PackageConfig -> ConfigFlags -> IO LocalBuildInfo
configure pkgconfig (maybe_hc_flavor, maybe_hc_path, maybe_prefix)
  = do
	-- prefix
	let prefix = case maybe_prefix of
			Just path -> path
			Nothing   -> system_default_prefix pkgconfig

        message $ "Using build prefix: " ++ prefix
	-- detect compiler
	compiler@(Compiler f' p' pkg) <- configCompiler maybe_hc_flavor maybe_hc_path pkgconfig 
        message $ "Using compiler flavor: " ++ (show f')
        message $ "Using compiler: " ++ p'
        message $ "Using package tool: " ++ pkg
	return LocalBuildInfo{prefix=prefix, compiler=compiler}

system_default_prefix PackageConfig{package=package} = 
#ifdef mingw32_TARGET_OS
  "C:\Program Files\" ++ pkgName package
#else
  "/usr"
#endif

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> PackageConfig
  -> IO Compiler

configCompiler (Just flavor) (Just path) pkgconfig
  = do pkgtool <- guessPkgToolFromHCPath flavor path
       return (Compiler{compilerFlavor=flavor,
			compilerPath=path,
			compilerPkgTool=pkgtool})

configCompiler (Just flavor) Nothing pkgconfig
  = do path <- findCompiler flavor
       pkgtool <- guessPkgToolFromHCPath flavor path
       return (Compiler{compilerFlavor=flavor,
			compilerPath=path,
			compilerPkgTool=pkgtool})

configCompiler Nothing maybe_path pkgconfig
  = configCompiler (Just defaultCompilerFlavor) maybe_path pkgconfig

defaultCompilerFlavor =
#if defined(__GLASGOW_HASKELL__)
   GHC
#elif defined(__NHC__)
   NHC
#elif defined(__HUGS__)
   Hugs
#endif

findCompiler :: CompilerFlavor -> IO FilePath
findCompiler flavor = findBinary (compilerBinaryName flavor)
   -- ToDo: check that compiler works? check compiler version?

compilerBinaryName GHC  = "ghc"
compilerBinaryName NHC  = "nhc98"
compilerBinaryName Hugs = "hugs"

compilerPkgToolName GHC  = "ghc-pkg"
compilerPkgToolName NHC  = "nhc98-pkg"
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

findBinary :: String -> IO FilePath
findBinary binary = do
  path <- getEnv "PATH"
  message $ "searching for " ++ binary ++ " in path."
  search (parsePath path)
  where
    search :: [FilePath] -> IO FilePath
    search [] = die ("Cannot find compiler for " ++ binary)
    search (d:ds) = do
	let path = d ++ '/':binary
	b <- doesFileExist path
	if b then do message ("found " ++ binary ++ " at "++ path); return path
             else search ds

parsePath :: String -> [FilePath]
parsePath path = split pathSep path
  where
#ifdef mingw32_TARGET_OS
	pathSep = ';'
#else
	pathSep = ':'
#endif

message s = putStrLn $ "configure: " ++ s

-- -----------------------------------------------------------------------------
-- Tests

#ifdef DEBUG
hunitTests :: IO [Test]
hunitTests = do
   let simonMarGHCLoc = "/home/simonmar/fp/bin/i386-unknown-linux/ghc"
   simonMarGHC <- configure PackageConfig{} (Just GHC,
				       Just simonMarGHCLoc,
				       Nothing)
   return $ [TestLabel "Configure Testing" $ TestList [
	     "finding ghc, etc on simonMar's machine" ~: "failed" ~:
             (LocalBuildInfo "/usr" (Compiler GHC 
	                    simonMarGHCLoc
 			    (simonMarGHCLoc ++ "-pkg")))
             ~=? simonMarGHC]]
#endif
