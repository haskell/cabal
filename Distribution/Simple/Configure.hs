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
                                      checkPersistBuildConfig,
                                      maybeGetPersistBuildConfig,
--                                      getConfiguredPkgDescr,
                                      localBuildInfoFile,
                                      getInstalledPackages,
				      configDependency,
                                      configCompiler, configCompilerAux,
                                      ccLdOptionsBuildInfo,
                                     )
    where

import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(..), compilerVersion, showCompilerId
    , unsupportedExtensions, PackageDB(..) )
import Distribution.Package
    ( PackageIdentifier(..), showPackageId )
import Distribution.PackageDescription
    ( PackageDescription(..), GenericPackageDescription(..)
    , Library(..), hasLibs, Executable(..), BuildInfo(..)
    , HookedBuildInfo, updatePackageDescription
    , allBuildInfo, emptyBuildInfo, unionBuildInfo )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription, satisfyDependency )
import Distribution.PackageDescription.Check
    ( PackageCheck(..), checkPackage, checkPackageFiles )
import Distribution.ParseUtils
    ( showDependency )
import Distribution.Simple.Program
    ( Program(..), ProgramLocation(..), ConfiguredProgram(..)
    , ProgramConfiguration, defaultProgramConfiguration
    , configureAllKnownPrograms, knownPrograms
    , userSpecifyArgs, userSpecifyPath
    , lookupKnownProgram, requireProgram, pkgConfigProgram
    , rawSystemProgramStdoutConf )
import Distribution.Simple.Setup
    ( ConfigFlags(..), CopyDest(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), defaultInstallDirs, combineInstallDirs )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(..), absoluteInstallDirs
    , prefixRelativeInstallDirs )
import Distribution.Simple.BuildPaths
    ( distPref )
import Distribution.Simple.Utils
    ( die, warn, info, setupMessage, createDirectoryIfMissingVerbose )
import Distribution.Simple.Register
    ( removeInstalledConfig )
import Distribution.System
    ( os, OS(..), Windows(..) )
import Distribution.Version
    ( Version(..), Dependency(..), VersionRange(..), showVersion, readVersion
    , showVersionRange, orLaterVersion, withinRange )
import Distribution.Verbosity
    ( Verbosity, lessVerbose )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Control.Monad
    ( when, unless, foldM )
import Control.Exception as Exception
    ( catch )
import Data.Char
    ( toLower )
import Data.List
    ( intersperse, nub, partition, isPrefixOf )
import Data.Maybe
    ( fromMaybe, isNothing )
import System.Directory
    ( doesFileExist, getModificationTime, createDirectoryIfMissing )
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

-- |Read the 'localBuildInfoFile'.  Error if it doesn't exist.  Also
-- fail if the file containing LocalBuildInfo is older than the .cabal
-- file, indicating that a re-configure is required.
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

-- |Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfig :: FilePath -> IO ()
checkPersistBuildConfig pkg_descr_file = do
  t0 <- getModificationTime pkg_descr_file
  t1 <- getModificationTime localBuildInfoFile
  when (t0 > t1) $
    die (pkg_descr_file ++ " has been changed, please re-configure.")

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
  = do  let verbosity = fromFlag (configVerbose cfg)

	setupMessage verbosity "Configuring"
                     (package (either packageDescription id pkg_descr0))

	createDirectoryIfMissingVerbose (lessVerbose verbosity) True distPref

        let programsConfig = 
                flip (foldr (uncurry userSpecifyArgs)) (configProgramArgs cfg)
              . flip (foldr (uncurry userSpecifyPath)) (configProgramPaths cfg)
              $ configPrograms cfg
            userInstall = fromFlag (configUserInstall cfg)
	    defaultPackageDB | userInstall = UserPackageDB
	                     | otherwise   = GlobalPackageDB
	    packageDb   = fromFlagOrDefault defaultPackageDB
	                                    (configPackageDB cfg)

	-- detect compiler
	(comp, programsConfig') <- configCompiler
          (flagToMaybe $ configHcFlavor cfg)
          (flagToMaybe $ configHcPath cfg) (flagToMaybe $ configHcPkg cfg)
          programsConfig (lessVerbose verbosity)
        let version = compilerVersion comp
            flavor  = compilerFlavor comp

        -- FIXME: currently only GHC has hc-pkg
        mipkgs <- getInstalledPackages (lessVerbose verbosity) comp
                    packageDb programsConfig'

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
              

        when (not (null flags)) $
          info verbosity $ "Flags chosen: " ++ (concat . intersperse ", " .
                      map (\(n,b) -> n ++ "=" ++ show b) $ flags)

        checkPackageProblems verbosity (updatePackageDescription pbi pkg_descr)

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
	defaultDirs <- defaultInstallDirs flavor userInstall (hasLibs pkg_descr)
	let installDirs = combineInstallDirs fromFlagOrDefault
                            defaultDirs (configInstallDirs cfg)

        -- check extensions
        let extlist = nub $ concatMap extensions (allBuildInfo pkg_descr)
        let exts = unsupportedExtensions comp extlist
        unless (null exts) $ warn verbosity $ -- Just warn, FIXME: Should this be an error?
            show flavor ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))

        let requiredBuildTools = concatMap buildTools (allBuildInfo pkg_descr)
        programsConfig'' <-
              configureAllKnownPrograms (lessVerbose verbosity) programsConfig'
          >>= configureRequiredPrograms verbosity requiredBuildTools
        
        (pkg_descr', programsConfig''') <- configurePkgconfigPackages verbosity
                                            pkg_descr programsConfig''

	split_objs <- 
	   if not (fromFlag $ configSplitObjs cfg)
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
		    scratchDir          = fromFlagOrDefault
                                            (distPref </> "scratch")
                                            (configScratchDir cfg),
		    packageDeps         = dep_pkgs,
                    pkgDescrFile        = Nothing,
		    localPkgDescr       = pkg_descr',
		    withPrograms        = programsConfig''',
		    withVanillaLib      = fromFlag $ configVanillaLib cfg,
		    withProfLib         = fromFlag $ configProfLib cfg,
		    withSharedLib       = fromFlag $ configSharedLib cfg,
		    withProfExe         = fromFlag $ configProfExe cfg,
		    withOptimization    = fromFlag $ configOptimization cfg,
		    withGHCiLib         = fromFlag $ configGHCiLib cfg,
		    splitObjs           = split_objs,
		    withPackageDB       = packageDb,
                    progPrefix          = fromFlag $ configProgPrefix cfg,
                    progSuffix          = fromFlag $ configProgSuffix cfg
                  }

        let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
            relative = prefixRelativeInstallDirs pkg_descr lbi

        info verbosity $ "Using compiler: " ++ showCompilerId comp
        info verbosity $ "Using install prefix: " ++ prefix dirs

        let dirinfo name dir isPrefixRelative =
              info verbosity $ name ++ " installed in: " ++ dir ++ relNote
              where relNote = case os of
                      Windows MingW | not (hasLibs pkg_descr)
                                   && isNothing isPrefixRelative
                                   -> "  (fixed location)"
                      _            -> ""

        dirinfo "Binaries"         (bindir dirs)     (bindir relative)
        dirinfo "Libraries"        (libdir dirs)     (libdir relative)
        dirinfo "Private binaries" (libexecdir dirs) (libexecdir relative)
        dirinfo "Data files"       (datadir dirs)    (datadir relative)
        dirinfo "Documentation"    (docdir dirs)     (docdir relative)

        sequence_ [ reportProgram verbosity prog configuredProg
                  | (prog, configuredProg) <- knownPrograms programsConfig' ]

	return lbi


-- -----------------------------------------------------------------------------
-- Configuring package dependencies

-- |Converts build dependencies to a versioned dependency.  only sets
-- version information for exact versioned dependencies.
setDepByVersion :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
setDepByVersion (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, just set it to empty
setDepByVersion (Dependency s _) = PackageIdentifier s (Version [] [])

reportProgram :: Verbosity -> Program -> Maybe ConfiguredProgram -> IO ()
reportProgram verbosity prog Nothing
    = info verbosity $ "No " ++ programName prog ++ " found"
reportProgram verbosity prog (Just configuredProg)
    = info verbosity $ "Using " ++ programName prog ++ version ++ location
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
        Just pkg -> do info verbosity $ "Dependency " ++ pkgname
                                ++ showVersionRange vrange
                                ++ ": using " ++ showPackageId pkg
                       return pkg

getInstalledPackages :: Verbosity -> Compiler -> PackageDB -> ProgramConfiguration
                     -> IO (Maybe [PackageIdentifier])
getInstalledPackages verbosity comp packageDb progconf = do
  info verbosity "Reading installed packages..."
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
-- Configuring pkg-config package dependencies

configurePkgconfigPackages :: Verbosity -> PackageDescription
                           -> ProgramConfiguration
                           -> IO (PackageDescription, ProgramConfiguration)
configurePkgconfigPackages verbosity pkg_descr conf
  | null allpkgs = return (pkg_descr, conf)
  | otherwise    = do
    (_, conf') <- requireProgram (lessVerbose verbosity) pkgConfigProgram
                    (orLaterVersion $ Version [0,9,0] []) conf
    mapM_ requirePkg allpkgs
    lib'  <- updateLibrary (library pkg_descr)
    exes' <- mapM updateExecutable (executables pkg_descr)
    let pkg_descr' = pkg_descr { library = lib', executables = exes' }
    return (pkg_descr', conf')
        
  where 
    allpkgs = concatMap pkgconfigDepends (allBuildInfo pkg_descr)
    pkgconfig = rawSystemProgramStdoutConf (lessVerbose verbosity)
                  pkgConfigProgram conf

    requirePkg (Dependency pkg range) = do
      version <- pkgconfig ["--modversion", pkg]
                 `Exception.catch` \_ -> die notFound
      case readVersion version of
        Nothing -> die "parsing output of pkg-config --modversion failed"
        Just v | not (withinRange v range) -> die (badVersion v)
               | otherwise                 -> info verbosity (depSatisfied v)
      where 
        notFound     = "The pkg-config package " ++ pkg ++ versionRequirement
                    ++ " is required but it could not be found."
        badVersion v = "The pkg-config package " ++ pkg ++ versionRequirement
                    ++ " is required but the version installed on the"
                    ++ " system is version " ++ showVersion v
        depSatisfied v = "Dependency " ++ pkg ++ showVersionRange range
                      ++ ": using version " ++ showVersion v

        versionRequirement
          | range == AnyVersion = ""
          | otherwise           = " version " ++ showVersionRange range                            

    updateLibrary Nothing    = return Nothing
    updateLibrary (Just lib) = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (libBuildInfo lib))
      return $ Just lib { libBuildInfo = libBuildInfo lib `unionBuildInfo` bi }

    updateExecutable exe = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (buildInfo exe))
      return exe { buildInfo = buildInfo exe `unionBuildInfo` bi }

    pkgconfigBuildInfo :: [Dependency] -> IO BuildInfo
    pkgconfigBuildInfo pkgdeps = do
      let pkgs = nub [ pkg | Dependency pkg _ <- pkgdeps ]
      ccflags <- pkgconfig ("--cflags" : pkgs)
      ldflags <- pkgconfig ("--libs"   : pkgs)
      return (ccLdOptionsBuildInfo (words ccflags) (words ldflags))

-- | Makes a 'BuildInfo' from C compiler and linker flags.
--
-- This can be used with the output from configuration programs like pkg-config
-- and similar package-specific programs like mysql-config, freealut-config etc.
-- For example:
--
-- > ccflags <- rawSystemProgramStdoutConf verbosity prog conf ["--cflags"]
-- > ldflags <- rawSystemProgramStdoutConf verbosity prog conf ["--libs"]
-- > return (ccldOptionsBuildInfo (words ccflags) (words ldflags))
--
ccLdOptionsBuildInfo :: [String] -> [String] -> BuildInfo
ccLdOptionsBuildInfo cflags ldflags =
  let (includeDirs',  cflags')   = partition ("-I" `isPrefixOf`) cflags
      (extraLibs',    ldflags')  = partition ("-l" `isPrefixOf`) ldflags
      (extraLibDirs', ldflags'') = partition ("-L" `isPrefixOf`) ldflags'
  in emptyBuildInfo {
       includeDirs  = map (drop 2) includeDirs',
       extraLibs    = map (drop 2) extraLibs',
       extraLibDirs = map (drop 2) extraLibDirs',
       ccOptions    = cflags',
       ldOptions    = ldflags''
     }

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAux :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
configCompilerAux cfg = configCompiler (flagToMaybe $ configHcFlavor cfg)
                                       (flagToMaybe $ configHcPath cfg)
                                       (flagToMaybe $ configHcPkg cfg)
                                       defaultProgramConfiguration
                                       (fromFlag (configVerbose cfg))

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


-- | Output package check warnings and errors. Exit if any errors.
checkPackageProblems :: Verbosity -> PackageDescription -> IO ()
checkPackageProblems verbosity pkg_descr = do
  ioChecks      <- checkPackageFiles pkg_descr "."
  let pureChecks = checkPackage      pkg_descr
      errors   = [ e | PackageBuildImpossible e <- pureChecks ++ ioChecks ]
      warnings = [ w | PackageBuildWarning    w <- pureChecks ++ ioChecks ]
  if null errors
    then mapM_ (warn verbosity) warnings
    else do mapM_ (hPutStrLn stderr . ("Error: " ++)) errors
            exitWith (ExitFailure 1)

-- -----------------------------------------------------------------------------
-- Tests

{- Too specific:
hunitTests :: [Test]
hunitTests = []
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
