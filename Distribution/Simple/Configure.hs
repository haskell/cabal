{-# OPTIONS -cpp #-}
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
-- Outputs the @dist\/setup-config@ file.

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

module Distribution.Simple.Configure (configure,
                                      writePersistBuildConfig,
                                      getPersistBuildConfig,
                                      maybeGetPersistBuildConfig,
--                                      getConfiguredPkgDescr,
                                      localBuildInfoFile,
                                      getInstalledPackages,
				      configDependency,
                                      configCompiler, configCompilerAux,
#ifdef DEBUG
                                      hunitTests
#endif
                                     )
    where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Compat.Directory
    ( createDirectoryIfMissing )
import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(..), compilerVersion, showCompilerId
    , unsupportedExtensions, PackageDB(..) )
import Distribution.Package
    ( PackageIdentifier(..), showPackageId )
import Distribution.PackageDescription
    ( PackageDescription(..), GenericPackageDescription(..)
    , BuildInfo(..), finalizePackageDescription
    , HookedBuildInfo, sanityCheckPackage, updatePackageDescription
    , setupMessage, satisfyDependency, hasLibs, allBuildInfo )
import Distribution.ParseUtils
    ( showDependency )
import Distribution.Simple.Program
    ( Program(..), ProgramLocation(..), ConfiguredProgram(..)
    , ProgramConfiguration, configureAllKnownPrograms, knownPrograms
    , lookupKnownProgram, requireProgram )

import Distribution.Simple.Setup
    ( ConfigFlags(..), CopyDest(..) )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), InstallDirTemplates(..), defaultInstallDirs
    , toPathTemplate )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(..), distPref, absoluteInstallDirs
    , prefixRelativeInstallDirs )
import Distribution.Simple.Utils
    ( die, warn )
import Distribution.Simple.Register
    ( removeInstalledConfig )
import Distribution.System
    ( os, OS(..), Windows(..) )
import Distribution.Version
    ( Version(..), Dependency(..), VersionRange(ThisVersion), showVersion
    , showVersionRange )
import Distribution.Verbosity
    ( Verbosity, verbose, lessVerbose )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Control.Monad
    ( when, unless, foldM )
import Data.Char
    ( toLower )
import Data.List
    ( intersperse, nub )
import Data.Maybe
    ( fromMaybe, isNothing )
import System.Directory
    ( doesFileExist )
import System.Environment
    ( getProgName )
import System.Exit
    ( ExitCode(..), exitWith )
import System.FilePath
    ( (</>) )
import qualified System.Info
    ( os, arch )
import System.IO
    ( hPutStrLn, stderr )
import Text.PrettyPrint.HughesPJ
    ( comma, punctuate, render, nest, sep )
    
import Prelude hiding (catch)

#ifdef DEBUG
import Test.HUnit
#endif

tryGetConfigStateFile :: (Read a) => FilePath -> IO (Either String a)
tryGetConfigStateFile filename = do
  e <- doesFileExist filename
  let dieMsg = "error reading " ++ filename ++ 
               "; run \"setup configure\" command?\n"
  if (not e) then return $ Left dieMsg else do 
    str <- readFile filename
    case reads str of
      [(bi,_)] -> return $ Right bi
      _        -> return $ Left  dieMsg

-- internal function
tryGetPersistBuildConfig :: IO (Either String LocalBuildInfo)
tryGetPersistBuildConfig = tryGetConfigStateFile localBuildInfoFile

-- |Read the 'localBuildInfoFile'.  Error if it doesn't exist.
getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  lbi <- tryGetPersistBuildConfig
  either die return lbi

-- |Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig = do
  lbi <- tryGetPersistBuildConfig
  return $ either (const Nothing) Just lbi

-- |After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = do
  createDirectoryIfMissing False distPref
  writeFile localBuildInfoFile (show lbi)

-- |@dist\/setup-config@
localBuildInfoFile :: FilePath
localBuildInfoFile = distPref </> "setup-config"


-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: ( Either GenericPackageDescription PackageDescription
             , HookedBuildInfo) 
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg
  = do  let verbosity = configVerbose cfg
            cfg' = cfg { configVerbose = lessVerbose verbosity }

	setupMessage verbosity "Configuring"
                     (either packageDescription id pkg_descr0)

	-- detect compiler
	(comp, programsConfig) <- configCompilerAux cfg'
        let version = compilerVersion comp
            flavor  = compilerFlavor comp

        -- FIXME: currently only GHC has hc-pkg
        mipkgs <- getInstalledPackages verbosity comp (configPackageDB cfg)
                    programsConfig

        (pkg_descr, flags) <- case pkg_descr0 of
            Left ppd -> 
                case finalizePackageDescription 
                       (configConfigurationsFlags cfg)
                       mipkgs
                       System.Info.os
                       System.Info.arch
                       (map toLower (show flavor),version)
                       ppd
                of Right r -> return r
                   Left missing -> 
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma $ 
                             map showDependency missing)
            Right pd -> return (pd,[])
              

        when (not (null flags) && verbosity >= verbose) $
          message $ "Flags chosen: " ++ (concat . intersperse ", " .
                      map (\(n,b) -> n ++ "=" ++ show b) $ flags)

        (warns, ers) <- sanityCheckPackage $
                          updatePackageDescription pbi pkg_descr
        
        errorOut verbosity warns ers

        let ipkgs = fromMaybe (map setDepByVersion (buildDepends pkg_descr)) mipkgs 

        dep_pkgs <- case flavor of
                      GHC | version >= Version [6,3] [] -> do
	                mapM (configDependency verbosity ipkgs) (buildDepends pkg_descr)
                      JHC                           -> do
	                mapM (configDependency verbosity ipkgs) (buildDepends pkg_descr)
                      _                             -> do
                        return $ map setDepByVersion (buildDepends pkg_descr)


	removeInstalledConfig

	-- installation directories
	defaultDirs <- defaultInstallDirs flavor (hasLibs pkg_descr)
	let maybeDefault confField dirField =
	      maybe (dirField defaultDirs) toPathTemplate (confField cfg)
	    installDirs = defaultDirs {
	        prefixDirTemplate  = maybeDefault configPrefix     prefixDirTemplate,
		binDirTemplate     = maybeDefault configBinDir     binDirTemplate,
		libDirTemplate     = maybeDefault configLibDir     libDirTemplate,
		libSubdirTemplate  = maybeDefault configLibSubDir  libSubdirTemplate,
		libexecDirTemplate = maybeDefault configLibExecDir libexecDirTemplate,
		dataDirTemplate    = maybeDefault configDataDir    dataDirTemplate,
		dataSubdirTemplate = maybeDefault configDataSubDir dataSubdirTemplate,
		docDirTemplate     = maybeDefault configDocDir     docDirTemplate
--		htmlDirTemplate    =
	      }

        -- check extensions
        let extlist = nub $ concatMap extensions (allBuildInfo pkg_descr)
        let exts = unsupportedExtensions comp extlist
        unless (null exts) $ warn verbosity $ -- Just warn, FIXME: Should this be an error?
            show flavor ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))

        let requiredBuildTools = concatMap buildTools (allBuildInfo pkg_descr)
        programsConfig' <-
              configureAllKnownPrograms (lessVerbose verbosity) programsConfig
          >>= configureRequiredPrograms verbosity requiredBuildTools

	split_objs <- 
	   if not (configSplitObjs cfg)
		then return False
		else case flavor of
			    GHC | version >= Version [6,5] [] -> return True
	    		    _ -> do warn verbosity
                                         ("this compiler does not support " ++
					  "--enable-split-objs; ignoring")
				    return False

	let lbi = LocalBuildInfo{
		    installDirTemplates = installDirs,
		    compiler            = comp,
		    buildDir            = distPref </> "build",
		    scratchDir          = distPref </> "scratch",
		    packageDeps         = dep_pkgs,
		    localPkgDescr       = pkg_descr,
		    withPrograms        = programsConfig',
		    withVanillaLib      = configVanillaLib cfg,
		    withProfLib         = configProfLib cfg,
		    withSharedLib       = configSharedLib cfg,
		    withProfExe         = configProfExe cfg,
		    withOptimization    = configOptimization cfg,
		    withGHCiLib         = configGHCiLib cfg,
		    splitObjs           = split_objs,
		    withPackageDB       = configPackageDB cfg
                  }

        let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
            relative = prefixRelativeInstallDirs pkg_descr lbi

        when (verbosity >= verbose) $ do
          message $ "Using compiler: " ++ showCompilerId comp
          message $ "Using install prefix: " ++ prefix dirs

          messageDir "Binaries"         pkg_descr (bindir dirs)    (bindir relative)
          messageDir "Libraries"        pkg_descr (libdir dirs)    (libdir relative)
          messageDir "Private binaries" pkg_descr (libexecdir dirs)(libexecdir relative)
          messageDir "Data files"       pkg_descr (datadir dirs)   (datadir relative)
          messageDir "Documentation"    pkg_descr (docdir dirs)    (docdir relative)

          sequence_ [ reportProgram prog configuredProg
                    | (prog, configuredProg) <- knownPrograms programsConfig' ]

	return lbi

messageDir :: String -> PackageDescription -> FilePath -> Maybe FilePath -> IO ()
messageDir name pkg_descr dir isPrefixRelative
 = message (name ++ " installed in: " ++ dir ++ rel_note)
  where
    rel_note = case os of
      Windows MingW | not (hasLibs pkg_descr)
                   && isNothing isPrefixRelative -> "  (fixed location)"
      _                                          -> ""

-- -----------------------------------------------------------------------------
-- Configuring package dependencies

-- |Converts build dependencies to a versioned dependency.  only sets
-- version information for exact versioned dependencies.
setDepByVersion :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
setDepByVersion (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, just set it to empty
setDepByVersion (Dependency s _) = PackageIdentifier s (Version [] [])

reportProgram :: Program -> Maybe ConfiguredProgram -> IO ()
reportProgram prog Nothing
    = message $ "No " ++ programName prog ++ " found"
reportProgram prog (Just configuredProg)
    = message $ "Using " ++ programName prog ++ version ++ location
    where location = case programLocation configuredProg of
            FoundOnSystem p -> " found on system at: " ++ p
            UserSpecified p -> " given by user at: " ++ p
          version = case programVersion configuredProg of
            Nothing -> ""
            Just v  -> " version " ++ showVersion v

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/"

-- | Test for a package dependency and record the version we have installed.
configDependency :: Verbosity -> [PackageIdentifier] -> Dependency -> IO PackageIdentifier
configDependency verbosity ps dep@(Dependency pkgname vrange) =
  case satisfyDependency ps dep of
        Nothing -> die $ "cannot satisfy dependency "
                      ++ pkgname ++ showVersionRange vrange ++ "\n"
                      ++ "Perhaps you need to download and install it from\n"
                      ++ hackageUrl ++ pkgname ++ "?"
        Just pkg -> do when (verbosity >= verbose) $
                         message $ "Dependency " ++ pkgname
                                ++ showVersionRange vrange
                                ++ ": using " ++ showPackageId pkg
                       return pkg

getInstalledPackages :: Verbosity -> Compiler -> PackageDB -> ProgramConfiguration
                     -> IO (Maybe [PackageIdentifier])
getInstalledPackages verbosity comp packageDb progconf = do
  when (verbosity >= verbose) $ message "Reading installed packages..."
  case compilerFlavor comp of
    GHC | compilerVersion comp >= Version [6,3] []
        -> Just `fmap` GHC.getInstalledPackages verbosity packageDb progconf
    JHC -> Just `fmap` JHC.getInstalledPackages verbosity packageDb progconf
    _   -> return Nothing

-- -----------------------------------------------------------------------------
-- Configuring program dependencies

configureRequiredPrograms :: Verbosity -> [Dependency] -> ProgramConfiguration -> IO ProgramConfiguration
configureRequiredPrograms verbosity deps conf =
  foldM (configureRequiredProgram verbosity) conf deps

configureRequiredProgram :: Verbosity -> ProgramConfiguration -> Dependency -> IO ProgramConfiguration
configureRequiredProgram verbosity conf (Dependency progName verRange) =
  case lookupKnownProgram progName conf of
    Nothing -> die ("Unknown build tool " ++ show progName)
    Just prog -> snd `fmap` requireProgram verbosity prog verRange conf

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAux :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
configCompilerAux cfg = configCompiler (configHcFlavor cfg)
                                       (configHcPath cfg)
                                       (configHcPkg cfg)
                                       (configPrograms cfg)
                                       (configVerbose cfg)

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> ProgramConfiguration -> Verbosity
               -> IO (Compiler, ProgramConfiguration)
configCompiler Nothing _ _ _ _ = die "Unknown compiler"
configCompiler (Just hcFlavor) hcPath hcPkg conf verbosity = do
  case hcFlavor of
      GHC  -> GHC.configure  verbosity hcPath hcPkg conf
      JHC  -> JHC.configure  verbosity hcPath hcPkg conf
      Hugs -> Hugs.configure verbosity hcPath hcPkg conf
      NHC  -> NHC.configure  verbosity hcPath hcPkg conf
      _    -> die "Unknown compiler"

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s


-- |Output warnings and errors. Exit if any errors.
errorOut :: Verbosity -- ^Verbosity
         -> [String]  -- ^Warnings
         -> [String]  -- ^errors
         -> IO ()
errorOut verbosity warnings errors = do
  mapM_ (warn verbosity) warnings
  when (not (null errors)) $ do
    pname <- getProgName
    mapM (hPutStrLn stderr . ((pname ++ ": Error: ") ++)) errors
    exitWith (ExitFailure 1)


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
