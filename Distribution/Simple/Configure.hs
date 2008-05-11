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
                                      tryGetConfigStateFile,
                                     )
    where

import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(compilerId), compilerFlavor, compilerVersion
    , showCompilerId, unsupportedExtensions, PackageDB(..) )
import Distribution.Package
    ( PackageIdentifier(PackageIdentifier), packageVersion, Package(..)
    , Dependency(Dependency) )
import Distribution.InstalledPackageInfo
    ( InstalledPackageInfo, emptyInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
    ( InstalledPackageInfo_(package,depends) )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.PackageDescription as PD
    ( PackageDescription(..), GenericPackageDescription(..)
    , Library(..), hasLibs, Executable(..), BuildInfo(..)
    , HookedBuildInfo, updatePackageDescription, allBuildInfo
    , FlagName(..) )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription )
import Distribution.PackageDescription.Check
    ( PackageCheck(..)
    , checkPackage, checkConfiguredPackage, checkPackageFiles )
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
import Distribution.Simple.Utils
    ( die, warn, info, setupMessage, createDirectoryIfMissingVerbose
    , intercalate, comparing, cabalVersion, cabalBootstrapping )
import Distribution.Simple.Register
    ( removeInstalledConfig )
import Distribution.System
    ( OS(..), buildOS, buildArch )
import Distribution.Version
    ( Version(..), VersionRange(..), orLaterVersion, withinRange )
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
import Data.List
    ( nub, partition, isPrefixOf, maximumBy )
import Data.Maybe
    ( fromMaybe, isNothing )
import Data.Monoid
    ( Monoid(..) )
import System.Directory
    ( doesFileExist, getModificationTime, createDirectoryIfMissing )
import System.Exit
    ( ExitCode(..), exitWith )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, stderr, hGetContents, openFile, hClose, IOMode(ReadMode) )
import Distribution.Text
    ( Text(disp), display, simpleParse )
import Text.PrettyPrint.HughesPJ
    ( comma, punctuate, render, nest, sep )
    
import Prelude hiding (catch)

tryGetConfigStateFile :: (Read a) => FilePath -> IO (Either String a)
tryGetConfigStateFile filename = do
  exists <- doesFileExist filename
  if not exists
    then return (Left missing)
    else do
      str <- readFileStrict filename
      return $ case lines str of
        [headder, rest] -> case checkHeader headder of
          Just msg -> Left msg
          Nothing  -> case reads rest of
            [(bi,_)] -> Right bi
            _        -> Left cantParse
        _            -> Left cantParse
  where
    readFileStrict name = do 
      h <- openFile name ReadMode
      str <- hGetContents h >>= \str -> length str `seq` return str 
      hClose h
      return str
    checkHeader :: String -> Maybe String
    checkHeader header = case parseHeader header of
      Just (cabalId, compId)
        | cabalId
       == currentCabalId -> Nothing
        | otherwise      -> Just (badVersion cabalId compId)
      Nothing            -> Just cantParse

    missing   = "Run the 'configure' command first."
    cantParse = "Saved package config file seems to be corrupt. "
             ++ "Try re-running the 'configure' command."
    badVersion cabalId compId
              = "You need to re-run the 'configure' command. "
             ++ "The version of Cabal being used has changed (was "
             ++ display cabalId ++ ", now "
             ++ display currentCabalId ++ ")."
             ++ badcompiler compId
    badcompiler compId | compId == currentCompilerId = ""
                       | otherwise
              = " Additionally the compiler is different (was "
             ++ display compId ++ ", now "
             ++ display currentCompilerId
             ++ ") which is probably the cause of the problem."

-- internal function
tryGetPersistBuildConfig :: FilePath -> IO (Either String LocalBuildInfo)
tryGetPersistBuildConfig distPref
    = tryGetConfigStateFile (localBuildInfoFile distPref)

-- |Read the 'localBuildInfoFile'.  Error if it doesn't exist.  Also
-- fail if the file containing LocalBuildInfo is older than the .cabal
-- file, indicating that a re-configure is required.
getPersistBuildConfig :: FilePath -> IO LocalBuildInfo
getPersistBuildConfig distPref = do
  lbi <- tryGetPersistBuildConfig distPref
  either die return lbi

-- |Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: FilePath -> IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig distPref = do
  lbi <- tryGetPersistBuildConfig distPref
  return $ either (const Nothing) Just lbi

-- |After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: FilePath -> LocalBuildInfo -> IO ()
writePersistBuildConfig distPref lbi = do
  createDirectoryIfMissing False distPref
  writeFile (localBuildInfoFile distPref)
            (showHeader pkgid ++ '\n' : show lbi)
  where
    pkgid   = packageId (localPkgDescr lbi)

showHeader :: PackageIdentifier -> String
showHeader pkgid =
     "Saved package config for " ++ display pkgid
  ++ " written by " ++ display currentCabalId
  ++      " using " ++ display currentCompilerId
  where

currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier "Cabal" currentVersion
  where currentVersion | cabalBootstrapping = Version [0] []
                       | otherwise          = cabalVersion

currentCompilerId :: PackageIdentifier
currentCompilerId = PackageIdentifier System.Info.compilerName
                                      System.Info.compilerVersion

parseHeader :: String -> Maybe (PackageIdentifier, PackageIdentifier) 
parseHeader header = case words header of
  ["Saved", "package", "config", "for", pkgid,
   "written", "by", cabalid, "using", compilerid]
    -> case (simpleParse pkgid :: Maybe PackageIdentifier,
             simpleParse cabalid,
             simpleParse compilerid) of
        (Just _,
         Just cabalid',
         Just compilerid') -> Just (cabalid', compilerid')
        _                  -> Nothing
  _                        -> Nothing

-- |Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfig :: FilePath -> FilePath -> IO ()
checkPersistBuildConfig distPref pkg_descr_file = do
  t0 <- getModificationTime pkg_descr_file
  t1 <- getModificationTime $ localBuildInfoFile distPref
  when (t0 > t1) $
    die (pkg_descr_file ++ " has been changed, please re-configure.")

-- |@dist\/setup-config@
localBuildInfoFile :: FilePath -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: ( Either GenericPackageDescription PackageDescription
             , HookedBuildInfo) 
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg
  = do  let distPref = fromFlag (configDistPref cfg)
            verbosity = fromFlag (configVerbosity cfg)

	setupMessage verbosity "Configuring"
                     (packageId (either packageDescription id pkg_descr0))

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
        maybePackageIndex <- getInstalledPackages (lessVerbose verbosity) comp
                               packageDb programsConfig'

        (pkg_descr0', flags) <- case pkg_descr0 of
            Left ppd -> 
                case finalizePackageDescription 
                       (configConfigurationsFlags cfg)
                       maybePackageIndex
                       Distribution.System.buildOS
                       Distribution.System.buildArch
                       (compilerId comp)
                       (configConstraints cfg)
                       ppd
                of Right r -> return r
                   Left missing -> 
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma $ 
                             map disp missing)
            Right pd -> return (pd,[])
              
        -- add extra include/lib dirs as specified in cfg
        -- we do it here so that those get checked too
        let pkg_descr = addExtraIncludeLibDirs pkg_descr0'

        when (not (null flags)) $
          info verbosity $ "Flags chosen: "
                        ++ intercalate ", " [ name ++ "=" ++ display value
                                            | (FlagName name, value) <- flags ]

        checkPackageProblems verbosity
          (either Just (\_->Nothing) pkg_descr0) --TODO: make the Either go away
          (updatePackageDescription pbi pkg_descr)

        let packageIndex = fromMaybe bogusPackageIndex maybePackageIndex
            -- FIXME: For Hugs, nhc98 and other compilers we do not know what
            -- packages are already installed, so we just make some up, pretend
            -- that they do exist and just hope for the best. We make them up
            -- based on what other package the package we're currently building
            -- happens to depend on. See 'inventBogusPackageId' below.
            -- Let's hope they really are installed... :-)
            bogusDependencies = map inventBogusPackageId (buildDepends pkg_descr)
            bogusPackageIndex = PackageIndex.fromList
              [ emptyInstalledPackageInfo {
                  InstalledPackageInfo.package = bogusPackageId
                  -- note that these bogus packages have no other dependencies
                }
              | bogusPackageId <- bogusDependencies ]
        dep_pkgs <- case flavor of
          GHC -> mapM (configDependency verbosity packageIndex) (buildDepends pkg_descr)
          JHC -> mapM (configDependency verbosity packageIndex) (buildDepends pkg_descr)
          _   -> return bogusDependencies

        packageDependsIndex <-
          case PackageIndex.dependencyClosure packageIndex dep_pkgs of
            Left packageDependsIndex -> return packageDependsIndex
            Right broken ->
              die $ "The following installed packages are broken because other"
                 ++ " packages they depend on are missing. These broken "
                 ++ "packages must be rebuilt before they can be used.\n"
                 ++ unlines [ "package "
                           ++ display (packageId pkg)
                           ++ " is broken due to missing package "
                           ++ intercalate ", " (map display deps)
                            | (pkg, deps) <- broken ]

        let pseudoTopPkg = emptyInstalledPackageInfo {
                InstalledPackageInfo.package = packageId pkg_descr,
                InstalledPackageInfo.depends = dep_pkgs
              }
        case PackageIndex.dependencyInconsistencies
           . PackageIndex.insert pseudoTopPkg
           $ packageDependsIndex of
          [] -> return ()
          inconsistencies ->
            warn verbosity $
                 "This package indirectly depends on multiple versions of the same "
              ++ "package. This is highly likely to cause a compile failure.\n"
              ++ unlines [ "package " ++ display pkg ++ " requires "
                        ++ display (PackageIdentifier name ver)
                         | (name, uses) <- inconsistencies
                         , (pkg, ver) <- uses ]

	removeInstalledConfig distPref

	-- installation directories
	defaultDirs <- defaultInstallDirs flavor userInstall (hasLibs pkg_descr)
	let installDirs = combineInstallDirs fromFlagOrDefault
                            defaultDirs (configInstallDirs cfg)

        -- check extensions
        let extlist = nub $ concatMap extensions (allBuildInfo pkg_descr)
        let exts = unsupportedExtensions comp extlist
        unless (null exts) $ warn verbosity $ -- Just warn, FIXME: Should this be an error?
            display flavor ++ " does not support the following extensions: " ++
            intercalate ", " (map display exts)

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
                    installedPkgs       = packageDependsIndex,
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
                    stripExes           = fromFlag $ configStripExes cfg,
		    withPackageDB       = packageDb,
                    progPrefix          = fromFlag $ configProgPrefix cfg,
                    progSuffix          = fromFlag $ configProgSuffix cfg
                  }

        let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
            relative = prefixRelativeInstallDirs pkg_descr lbi

        unless (isAbsolute (prefix dirs)) $ die $
            "expected an absolute directory name for --prefix: " ++ prefix dirs

        info verbosity $ "Using " ++ display currentCabalId
                      ++ " compiled by " ++ display currentCompilerId
        info verbosity $ "Using compiler: " ++ showCompilerId comp
        info verbosity $ "Using install prefix: " ++ prefix dirs

        let dirinfo name dir isPrefixRelative =
              info verbosity $ name ++ " installed in: " ++ dir ++ relNote
              where relNote = case buildOS of
                      Windows | not (hasLibs pkg_descr)
                             && isNothing isPrefixRelative
                             -> "  (fixed location)"
                      _      -> ""

        dirinfo "Binaries"         (bindir dirs)     (bindir relative)
        dirinfo "Libraries"        (libdir dirs)     (libdir relative)
        dirinfo "Private binaries" (libexecdir dirs) (libexecdir relative)
        dirinfo "Data files"       (datadir dirs)    (datadir relative)
        dirinfo "Documentation"    (docdir dirs)     (docdir relative)

        sequence_ [ reportProgram verbosity prog configuredProg
                  | (prog, configuredProg) <- knownPrograms programsConfig''' ]

	return lbi

    where
      addExtraIncludeLibDirs pkg_descr =
          let extraBi = mempty { extraLibDirs = configExtraLibDirs cfg
                               , PD.includeDirs = configExtraIncludeDirs cfg}
              modifyLib l        = l{ libBuildInfo = libBuildInfo l `mappend` extraBi }
              modifyExecutable e = e{ buildInfo    = buildInfo e    `mappend` extraBi}
          in pkg_descr{ library     = modifyLib        `fmap` library pkg_descr
                      , executables = modifyExecutable  `map` executables pkg_descr}
-- -----------------------------------------------------------------------------
-- Configuring package dependencies

-- |Converts build dependencies to a versioned dependency.  only sets
-- version information for exact versioned dependencies.
inventBogusPackageId :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
inventBogusPackageId (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, just set it to empty
inventBogusPackageId (Dependency s _) = PackageIdentifier s (Version [] [])

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
            Just v  -> " version " ++ display v

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/"

-- | Test for a package dependency and record the version we have installed.
configDependency :: Verbosity -> PackageIndex InstalledPackageInfo -> Dependency -> IO PackageIdentifier
configDependency verbosity index dep@(Dependency pkgname vrange) =
  case PackageIndex.lookupDependency index dep of
        [] -> die $ "cannot satisfy dependency "
                      ++ pkgname ++ display vrange ++ "\n"
                      ++ "Perhaps you need to download and install it from\n"
                      ++ hackageUrl ++ pkgname ++ "?"
        pkgs -> do let pkgid = maximumBy (comparing packageVersion) (map packageId pkgs)
                   info verbosity $ "Dependency " ++ pkgname
                                ++ display vrange
                                ++ ": using " ++ display pkgid
                   return pkgid

getInstalledPackages :: Verbosity -> Compiler -> PackageDB -> ProgramConfiguration
                     -> IO (Maybe (PackageIndex InstalledPackageInfo))
getInstalledPackages verbosity comp packageDb progconf = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> Just `fmap` GHC.getInstalledPackages verbosity packageDb progconf
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
      case simpleParse version of
        Nothing -> die "parsing output of pkg-config --modversion failed"
        Just v | not (withinRange v range) -> die (badVersion v)
               | otherwise                 -> info verbosity (depSatisfied v)
      where 
        notFound     = "The pkg-config package " ++ pkg ++ versionRequirement
                    ++ " is required but it could not be found."
        badVersion v = "The pkg-config package " ++ pkg ++ versionRequirement
                    ++ " is required but the version installed on the"
                    ++ " system is version " ++ display v
        depSatisfied v = "Dependency " ++ pkg ++ display range
                      ++ ": using version " ++ display v

        versionRequirement
          | range == AnyVersion = ""
          | otherwise           = " version " ++ display range

    updateLibrary Nothing    = return Nothing
    updateLibrary (Just lib) = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (libBuildInfo lib))
      return $ Just lib { libBuildInfo = libBuildInfo lib `mappend` bi }

    updateExecutable exe = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (buildInfo exe))
      return exe { buildInfo = buildInfo exe `mappend` bi }

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
  in mempty {
       PD.includeDirs  = map (drop 2) includeDirs',
       PD.extraLibs    = map (drop 2) extraLibs',
       PD.extraLibDirs = map (drop 2) extraLibDirs',
       PD.ccOptions    = cflags',
       PD.ldOptions    = ldflags''
     }

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAux :: ConfigFlags -> IO (Compiler, ProgramConfiguration)
configCompilerAux cfg = configCompiler (flagToMaybe $ configHcFlavor cfg)
                                       (flagToMaybe $ configHcPath cfg)
                                       (flagToMaybe $ configHcPkg cfg)
                                       defaultProgramConfiguration
                                       (fromFlag (configVerbosity cfg))

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
checkPackageProblems :: Verbosity
                     -> Maybe GenericPackageDescription
                     -> PackageDescription
                     -> IO ()
checkPackageProblems verbosity mgpkg pkg = do
  ioChecks      <- checkPackageFiles pkg "."
  let pureChecks = case mgpkg of
                     Just gpkg -> checkPackage gpkg (Just pkg)
                     Nothing   -> checkConfiguredPackage pkg
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
