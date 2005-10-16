-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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
                                      findProgram,
                                      getInstalledPackages,
                                      configCompiler,
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

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Register (removeInstalledConfig)
import Distribution.Setup(ConfigFlags(..), CopyDest(..))
import Distribution.Compiler(CompilerFlavor(..), Compiler(..),
			     compilerBinaryName, extensionsToFlags)
import Distribution.Package (PackageIdentifier(..), showPackageId, 
			     parsePackageId)
import Distribution.PackageDescription(
 	PackageDescription(..), Library(..),
	BuildInfo(..), Executable(..), setupMessage)
import Distribution.Simple.Utils (die, warn, withTempFile,maybeExit)
import Distribution.Version (Version(..), Dependency(..), VersionRange(ThisVersion),
			     parseVersion, showVersion, withinRange,
			     showVersionRange)

import Data.List (intersperse, nub, maximumBy, isPrefixOf)
import Data.Char (isSpace)
import Data.Maybe(fromMaybe)
import System.Directory
import Distribution.Compat.FilePath (splitFileName, joinFileName,
                                  joinFileExt, exeExtension)
import Distribution.Program(Program(..), ProgramLocation(..), lookupProgram,
                            updateProgram, haddockProgram, pfesetupProgram)
import System.Cmd		( system )
import System.Exit		( ExitCode(..) )
import Control.Monad		( when, unless )
import Distribution.Compat.ReadP
import Distribution.Compat.Directory (findExecutable)
import Data.Char (isDigit)
import Prelude hiding (catch)


#ifdef DEBUG
import HUnit
#endif

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  e <- doesFileExist localBuildInfoFile
  let dieMsg = "error reading " ++ localBuildInfoFile ++ "; run \"setup configure\" command?\n"
  when (not e) (die dieMsg)
  str <- readFile localBuildInfoFile
  case reads str of
    [(bi,_)] -> return bi
    _        -> die dieMsg

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
	-- detect compiler
	comp@(Compiler f' ver p' pkg) <- configCompilerAux cfg

	-- installation directories
	defPrefix <- default_prefix
	defDataDir <- default_datadir pkg_descr
        let 
		pref = fromMaybe defPrefix (configPrefix cfg)
		my_bindir = fromMaybe default_bindir 
				  (configBinDir cfg)
		my_libdir = fromMaybe (default_libdir comp)
				  (configLibDir cfg)
		my_libsubdir = fromMaybe (default_libsubdir comp)
				  (configLibSubDir cfg)
		my_libexecdir = fromMaybe default_libexecdir
				  (configLibExecDir cfg)
		my_datadir = fromMaybe defDataDir
				  (configDataDir cfg)
		my_datasubdir = fromMaybe default_datasubdir
				  (configDataSubDir cfg)

	-- on Windows, our directories should all be relative to $prefix if we're
	-- building an executable, so we can be prefix-independent
#if mingw32_HOST_OS	
	let checkPrefix (s,('$':'p':'r':'e':'f':'i':'x':_)) = return ()
	    checkPrefix (s,_other) = 
		die (s ++ " must begin with $prefix for an executable")
	mapM_ checkPrefix [
		("bindir",my_bindir),
		("libdir",my_libdir),
		("libexecdir",my_libexecdir),
		("datadir",my_datadir)
	    ]
#endif

        -- check extensions
        let extlist = nub $ maybe [] (extensions . libBuildInfo) lib ++
                      concat [ extensions exeBi | Executable _ _ exeBi <- executables pkg_descr ]
        let exts = fst $ extensionsToFlags f' extlist
        unless (null exts) $ warn $ -- Just warn, FIXME: Should this be an error?
            show f' ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))
        let haddockName = programName haddockProgram
        -- FIX: just do a map over all of them :)
        haddock   <- lookupProgram haddockName (configPrograms cfg)
        pfe   <- lookupProgram (programName pfesetupProgram) (configPrograms cfg)

        happy     <- findProgram "happy"     (configHappy cfg)
        alex      <- findProgram "alex"      (configAlex cfg)
        hsc2hs    <- findProgram "hsc2hs"    (configHsc2hs cfg)
        c2hs      <- findProgram "c2hs"      (configC2hs cfg)
        cpphs     <- findProgram "cpphs"     (configCpphs cfg)
        greencard <- findProgram "greencard" (configGreencard cfg)
        -- FIXME: maybe this should only be printed when verbose?
        message $ "Using install prefix: " ++ pref
        message $ "Using compiler: " ++ p'
        message $ "Compiler flavor: " ++ (show f')
        message $ "Compiler version: " ++ showVersion ver
        message $ "Using package tool: " ++ pkg
        reportProgram "haddock"   haddock
        reportProgram "happy"     happy
        reportProgram "alex"      alex
        reportProgram "hsc2hs"    hsc2hs
        reportProgram "c2hs"      c2hs
        reportProgram "cpphs"     cpphs
        reportProgram "greencard" greencard
        -- FIXME: currently only GHC has hc-pkg
        dep_pkgs <- if f' == GHC && ver >= Version [6,3] [] then do
            ipkgs <-  getInstalledPackagesAux comp cfg
	    mapM (configDependency ipkgs) (buildDepends pkg_descr)
          else return $ map setDepByVersion (buildDepends pkg_descr)

	let lbi = LocalBuildInfo{prefix=pref, compiler=comp,
			      buildDir="dist" `joinFileName` "build",
			      bindir=my_bindir,
			      libdir=my_libdir,
			      libsubdir=my_libsubdir,
			      libexecdir=my_libexecdir,
			      datadir=my_datadir,
			      datasubdir=my_datasubdir,
                              packageDeps=dep_pkgs,
                              withPrograms=newConfig,
                              withHappy=happy, withAlex=alex,
                              withHsc2hs=hsc2hs, withC2hs=c2hs,
                              withCpphs=cpphs,
                              withGreencard=greencard,
                              withProfLib=configProfLib cfg,
                              withProfExe=configProfExe cfg,
			      withGHCiLib=configGHCiLib cfg
                             }

        -- FIXME: maybe this should only be printed when verbose?
        message $ "Using install prefix: " ++ pref
        message $ "Binaries installed in: " ++ mkBinDir pkg_descr lbi NoCopyDest
        message $ "Libraries installed in: " ++ mkLibDir pkg_descr lbi NoCopyDest
        message $ "Private binaries installed in: " ++ mkLibexecDir pkg_descr lbi NoCopyDest
        message $ "Data files installed in: " ++ mkDataDir pkg_descr lbi NoCopyDest
        message $ "Using compiler: " ++ p'
        message $ "Compiler flavor: " ++ (show f')
        message $ "Compiler version: " ++ showVersion ver
        message $ "Using package tool: " ++ pkg
        reportProgram "haddock"   haddock
        reportProgram "happy"     happy
        reportProgram "alex"      alex
        reportProgram "hsc2hs"    hsc2hs
        reportProgram "c2hs"      c2hs
        reportProgram "cpphs"     cpphs
        reportProgram "greencard" greencard

	return lbi

-- |Converts build dependencies to a versioned dependency.  only sets
-- version information for exact versioned dependencies.
setDepByVersion :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
setDepByVersion (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, just set it to empty
setDepByVersion (Dependency s _) = PackageIdentifier s (Version [] [])


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

reportProgram' :: String -> Maybe Program -> IO ()
reportProgram' _ (Just Program{ programName=name
                              , programLocation=EmptyLocation})
                  = message ("No " ++ name ++ " found")
reportProgram' _ (Just Program{ programName=name
                              , programLocation=FoundOnSystem p})
                  = message ("Using " ++ name ++ " found on system at: " ++ p)
reportProgram' _ (Just Program{ programName=name
                              , programLocation=UserSpecified p})
                  = message ("Using " ++ name ++ " given by user at: " ++ p)
reportProgram' name Nothing = message ("No " ++ name ++ " found")


-- | Test for a package dependency and record the version we have installed.
configDependency :: [PackageIdentifier] -> Dependency -> IO PackageIdentifier
configDependency ps (Dependency pkgname vrange) = do
  let
	ok p = pkgName p == pkgname && pkgVersion p `withinRange` vrange
  --
  case filter ok ps of
    [] -> die ("cannot satisfy dependency " ++ 
			pkgname ++ showVersionRange vrange)
    qs -> let 
	    pkg = maximumBy versions qs
	    versions a b = pkgVersion a `compare` pkgVersion b
	  in do message ("Dependency " ++ pkgname ++ showVersionRange vrange ++
			 ": using " ++ showPackageId pkg)
		return pkg

getInstalledPackagesAux :: Compiler -> ConfigFlags -> IO [PackageIdentifier]
getInstalledPackagesAux comp cfg = getInstalledPackages comp (configUser cfg) (configVerbose cfg)

getInstalledPackages :: Compiler -> Bool -> Int -> IO [PackageIdentifier]
getInstalledPackages comp user verbose = do
   when (verbose > 0) $ message "Reading installed packages..."
   withTempFile "." "" $ \tmp -> do
      let user_flag = if user then "--user" else "--global"
          cmd_line  = "\"" ++ compilerPkgTool comp ++ "\" " ++ user_flag ++ " list >" ++ tmp
      when (verbose > 0) $
        putStrLn cmd_line
      res <- system cmd_line
      case res of
        ExitFailure _ -> die ("cannot get package list")
        ExitSuccess -> do
	  str <- readFile tmp
	  let 
	      keep_line s = ':' `notElem` s && not ("Creating" `isPrefixOf` s)
	      str1 = unlines (filter keep_line (lines str))
	      str2 = filter (`notElem` ",()") str1
	  --
	  case pCheck (readP_to_S (many (skipSpaces >> parsePackageId)) str2) of
	    [ps] -> return ps
	    _   -> die "cannot parse package list"

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAux :: ConfigFlags -> IO Compiler
configCompilerAux cfg = configCompiler (configHcFlavor cfg)
                                       (configHcPath cfg)
                                       (configHcPkg cfg)
                                       (configVerbose cfg)

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath -> Int -> IO Compiler
configCompiler hcFlavor hcPath hcPkg verbose
  = do let flavor = case hcFlavor of
                      Just f  -> f
                      Nothing -> error "Unknown compiler"
       comp <- 
	 case hcPath of
	   Just path -> return path
	   Nothing   -> findCompiler verbose flavor

       ver <- configCompilerVersion flavor comp verbose

       pkgtool <-
	 case hcPkg of
	   Just path -> return path
	   Nothing   -> guessPkgToolFromHCPath verbose flavor comp

       return (Compiler{compilerFlavor=flavor,
			compilerVersion=ver,
			compilerPath=comp,
			compilerPkgTool=pkgtool})

findCompiler :: Int -> CompilerFlavor -> IO FilePath
findCompiler verbose flavor = do
  let prog = compilerBinaryName flavor
  when (verbose > 0) $ message $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
   Nothing   -> die ("Cannot find compiler for " ++ prog)
   Just path -> do when (verbose > 0) $ message ("found " ++ prog ++ " at "++ path)
		   return path
   -- ToDo: check that compiler works? check compiler version?

compilerPkgToolName :: CompilerFlavor -> String
compilerPkgToolName GHC  = "ghc-pkg"
compilerPkgToolName NHC  = "hmake" -- FIX: nhc98-pkg Does not yet exist
compilerPkgToolName Hugs = "hugs" -- FIX (HUGS): hugs-pkg does not yet exist
compilerPkgToolName cmp  = error $ "Unsupported compiler: " ++ (show cmp)

configCompilerVersion :: CompilerFlavor -> FilePath -> Int -> IO Version
configCompilerVersion GHC compilerP verbose =
  withTempFile "." "" $ \tmp -> do
    let cmd_line = "\"" ++ compilerP ++ "\" --version >" ++ tmp
    when (verbose > 0) $
      putStrLn cmd_line
    maybeExit $ system cmd_line
    str <- readFile tmp
    case pCheck (readP_to_S parseVersion (dropWhile (not.isDigit) str)) of
	[v] -> return v
	_   -> die ("cannot determine version of " ++ compilerP ++ ":\n  "
			++ str)
configCompilerVersion _ _ _ = return Version{ versionBranch=[],versionTags=[] }

pCheck :: [(a, [Char])] -> [a]
pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

guessPkgToolFromHCPath :: Int -> CompilerFlavor -> FilePath -> IO FilePath
guessPkgToolFromHCPath verbose flavor path
  = do let pkgToolName     = compilerPkgToolName flavor
           (dir,_)         = splitFileName path
           pkgtool         = dir `joinFileName` pkgToolName `joinFileExt` exeExtension
       when (verbose > 0) $ message $ "looking for package tool: " ++ pkgToolName ++ " near compiler in " ++ path
       exists <- doesFileExist pkgtool
       when (not exists) $
	  die ("Cannot find package tool: " ++ pkgtool)
       when (verbose > 0) $ message $ "found package tool in " ++ pkgtool
       return pkgtool

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s

-- -----------------------------------------------------------------------------
-- Tests

#ifdef DEBUG

hunitTests :: [Test]
hunitTests = []
{- Too specific:
packageID = PackageIdentifier "Foo" (Version [1] [])
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
