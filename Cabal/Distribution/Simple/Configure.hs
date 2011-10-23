-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This deals with the /configure/ phase. It provides the 'configure' action
-- which is given the package description and configure flags. It then tries
-- to: configure the compiler; resolves any conditionals in the package
-- description; resolve the package dependencies; check if all the extensions
-- used by this package are supported by the compiler; check that all the build
-- tools are available (including version checks if appropriate); checks for
-- any required @pkg-config@ packages (updating the 'BuildInfo' with the
-- results)
--
-- Then based on all this it saves the info in the 'LocalBuildInfo' and writes
-- it out to the @dist\/setup-config@ file. It also displays various details to
-- the user, the amount of information displayed depending on the verbosity
-- level.

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
                                      checkPersistBuildConfigOutdated,
                                      maybeGetPersistBuildConfig,
                                      localBuildInfoFile,
                                      getInstalledPackages,
                                      configCompiler, configCompilerAux,
                                      ccLdOptionsBuildInfo,
                                      tryGetConfigStateFile,
                                      checkForeignDeps,
                                     )
    where

import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(compilerId), compilerFlavor, compilerVersion
    , showCompilerId, unsupportedLanguages, unsupportedExtensions
    , PackageDB(..), PackageDBStack )
import Distribution.Package
    ( PackageName(PackageName), PackageIdentifier(..), PackageId
    , packageName, packageVersion, Package(..)
    , Dependency(Dependency), simplifyDependency
    , InstalledPackageId(..) )
import Distribution.InstalledPackageInfo as Installed
    ( InstalledPackageInfo, InstalledPackageInfo_(..)
    , emptyInstalledPackageInfo )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.PackageDescription as PD
    ( PackageDescription(..), specVersion, GenericPackageDescription(..)
    , Library(..), hasLibs, Executable(..), BuildInfo(..), allExtensions
    , HookedBuildInfo, updatePackageDescription, allBuildInfo
    , FlagName(..), TestSuite(..), Benchmark(..) )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription, mapTreeData )
import Distribution.PackageDescription.Check
    ( PackageCheck(..), checkPackage, checkPackageFiles )
import Distribution.Simple.Hpc ( enableCoverage )
import Distribution.Simple.Program
    ( Program(..), ProgramLocation(..), ConfiguredProgram(..)
    , ProgramConfiguration, defaultProgramConfiguration
    , configureAllKnownPrograms, knownPrograms, lookupKnownProgram
    , userSpecifyArgss, userSpecifyPaths
    , requireProgram, requireProgramVersion
    , pkgConfigProgram, gccProgram, rawSystemProgramStdoutConf )
import Distribution.Simple.Setup
    ( ConfigFlags(..), CopyDest(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), defaultInstallDirs, combineInstallDirs )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
    , absoluteInstallDirs, prefixRelativeInstallDirs, inplacePackageId
    , allComponentsBy, Component(..), foldComponent, ComponentName(..) )
import Distribution.Simple.BuildPaths
    ( autogenModulesDir )
import Distribution.Simple.Utils
    ( die, warn, info, setupMessage, createDirectoryIfMissingVerbose
    , intercalate, cabalVersion
    , withFileContents, writeFileAtomic
    , withTempFile )
import Distribution.System
    ( OS(..), buildOS, buildPlatform )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion, withinRange, isAnyVersion )
import Distribution.Verbosity
    ( Verbosity, lessVerbose )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.LHC  as LHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs
import qualified Distribution.Simple.UHC  as UHC

import Control.Monad
    ( when, unless, foldM, filterM, forM )
import Data.List
    ( nub, partition, isPrefixOf, inits, find )
import Data.Maybe
    ( isNothing, catMaybes, mapMaybe )
import Data.Monoid
    ( Monoid(..) )
import Data.Graph
    ( SCC(..), graphFromEdges, transposeG, vertices, stronglyConnCompR )
import System.Directory
    ( doesFileExist, getModificationTime, createDirectoryIfMissing, getTemporaryDirectory )
import System.Exit
    ( ExitCode(..), exitWith )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, stderr, hClose )
import Distribution.Text
    ( Text(disp), display, simpleParse )
import Text.PrettyPrint
    ( comma, punctuate, render, nest, sep )
import Distribution.Compat.Exception ( catchExit, catchIO )

import Prelude hiding (catch)

tryGetConfigStateFile :: (Read a) => FilePath -> IO (Either String a)
tryGetConfigStateFile filename = do
  exists <- doesFileExist filename
  if not exists
    then return (Left missing)
    else withFileContents filename $ \str ->
      case lines str of
        [headder, rest] -> case checkHeader headder of
          Just msg -> return (Left msg)
          Nothing  -> case reads rest of
            [(bi,_)] -> return (Right bi)
            _        -> return (Left cantParse)
        _            -> return (Left cantParse)
  where
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
  writeFileAtomic (localBuildInfoFile distPref)
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
currentCabalId = PackageIdentifier (PackageName "Cabal") cabalVersion

currentCompilerId :: PackageIdentifier
currentCompilerId = PackageIdentifier (PackageName System.Info.compilerName)
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
checkPersistBuildConfigOutdated :: FilePath -> FilePath -> IO Bool
checkPersistBuildConfigOutdated distPref pkg_descr_file = do
  t0 <- getModificationTime pkg_descr_file
  t1 <- getModificationTime $ localBuildInfoFile distPref
  return (t0 > t1)

-- |@dist\/setup-config@
localBuildInfoFile :: FilePath -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg
  = do  let distPref = fromFlag (configDistPref cfg)
            buildDir' = distPref </> "build"
            verbosity = fromFlag (configVerbosity cfg)

        setupMessage verbosity "Configuring" (packageId pkg_descr0)

        createDirectoryIfMissingVerbose (lessVerbose verbosity) True distPref

        let programsConfig = userSpecifyArgss (configProgramArgs cfg)
                           . userSpecifyPaths (configProgramPaths cfg)
                           $ configPrograms cfg
            userInstall = fromFlag (configUserInstall cfg)
            packageDbs = implicitPackageDbStack userInstall
                           (flagToMaybe $ configPackageDB cfg)

        -- detect compiler
        (comp, programsConfig') <- configCompiler
          (flagToMaybe $ configHcFlavor cfg)
          (flagToMaybe $ configHcPath cfg) (flagToMaybe $ configHcPkg cfg)
          programsConfig (lessVerbose verbosity)
        let version = compilerVersion comp
            flavor  = compilerFlavor comp

        -- Create a PackageIndex that makes *any libraries that might be*
        -- defined internally to this package look like installed packages, in
        -- case an executable should refer to any of them as dependencies.
        --
        -- It must be *any libraries that might be* defined rather than the
        -- actual definitions, because these depend on conditionals in the .cabal
        -- file, and we haven't resolved them yet.  finalizePackageDescription
        -- does the resolution of conditionals, and it takes internalPackageSet
        -- as part of its input.
        --
        -- Currently a package can define no more than one library (which has
        -- the same name as the package) but we could extend this later.
        -- If we later allowed private internal libraries, then here we would
        -- need to pre-scan the conditional data to make a list of all private
        -- libraries that could possibly be defined by the .cabal file.
        let pid = packageId pkg_descr0
            internalPackage = emptyInstalledPackageInfo {
                --TODO: should use a per-compiler method to map the source
                --      package ID into an installed package id we can use
                --      for the internal package set. The open-codes use of
                --      InstalledPackageId . display here is a hack.
                Installed.installedPackageId = InstalledPackageId $ display $ pid,
                Installed.sourcePackageId = pid
              }
            internalPackageSet = PackageIndex.fromList [internalPackage]
        installedPackageSet <- getInstalledPackages (lessVerbose verbosity) comp
                                      packageDbs programsConfig'

        let -- Constraint test function for the solver
            dependencySatisfiable =
                not . null . PackageIndex.lookupDependency pkgs'
              where
                pkgs' = PackageIndex.insert internalPackage installedPackageSet
            enableTest t = t { testEnabled = fromFlag (configTests cfg) }
            flaggedTests = map (\(n, t) -> (n, mapTreeData enableTest t))
                               (condTestSuites pkg_descr0)
            enableBenchmark bm = bm { benchmarkEnabled = fromFlag (configBenchmarks cfg) }
            flaggedBenchmarks = map (\(n, bm) -> (n, mapTreeData enableBenchmark bm))
                               (condBenchmarks pkg_descr0)
            pkg_descr0'' = pkg_descr0 { condTestSuites = flaggedTests
                                      , condBenchmarks = flaggedBenchmarks }

        (pkg_descr0', flags) <-
                case finalizePackageDescription
                       (configConfigurationsFlags cfg)
                       dependencySatisfiable
                       Distribution.System.buildPlatform
                       (compilerId comp)
                       (configConstraints cfg)
                       pkg_descr0''
                of Right r -> return r
                   Left missing ->
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma
                                    . map (disp . simplifyDependency)
                                    $ missing)

        -- add extra include/lib dirs as specified in cfg
        -- we do it here so that those get checked too
        let pkg_descr =
                enableCoverage (fromFlag (configLibCoverage cfg)) distPref
                $ addExtraIncludeLibDirs pkg_descr0'

        when (not (null flags)) $
          info verbosity $ "Flags chosen: "
                        ++ intercalate ", " [ name ++ "=" ++ display value
                                            | (FlagName name, value) <- flags ]

        checkPackageProblems verbosity pkg_descr0
          (updatePackageDescription pbi pkg_descr)

        let selectDependencies =
                (\xs -> ([ x | Left x <- xs ], [ x | Right x <- xs ]))
              . map (selectDependency internalPackageSet installedPackageSet)

            (failedDeps, allPkgDeps) = selectDependencies (buildDepends pkg_descr)

            internalPkgDeps = [ pkgid | InternalDependency _ pkgid <- allPkgDeps ]
            externalPkgDeps = [ pkg   | ExternalDependency _ pkg   <- allPkgDeps ]

        when (not (null internalPkgDeps) && not (newPackageDepsBehaviour pkg_descr)) $
            die $ "The field 'build-depends: "
               ++ intercalate ", " (map (display . packageName) internalPkgDeps)
               ++ "' refers to a library which is defined within the same "
               ++ "package. To use this feature the package must specify at "
               ++ "least 'cabal-version: >= 1.8'."

        reportFailedDependencies failedDeps
        reportSelectedDependencies verbosity allPkgDeps

        packageDependsIndex <-
          case PackageIndex.dependencyClosure installedPackageSet
                  (map Installed.installedPackageId externalPkgDeps) of
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
                Installed.installedPackageId = InstalledPackageId (display (packageId pkg_descr)),
                Installed.sourcePackageId = packageId pkg_descr,
                Installed.depends = map Installed.installedPackageId externalPkgDeps
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

        -- installation directories
        defaultDirs <- defaultInstallDirs flavor userInstall (hasLibs pkg_descr)
        let installDirs = combineInstallDirs fromFlagOrDefault
                            defaultDirs (configInstallDirs cfg)

        -- check languages and extensions
        let langlist = nub $ catMaybes $ map defaultLanguage (allBuildInfo pkg_descr)
        let langs = unsupportedLanguages comp langlist
        when (not (null langs)) $
          die $ "The package " ++ display (packageId pkg_descr0)
             ++ " requires the following languages which are not "
             ++ "supported by " ++ display (compilerId comp) ++ ": "
             ++ intercalate ", " (map display langs)
        let extlist = nub $ concatMap allExtensions (allBuildInfo pkg_descr)
        let exts = unsupportedExtensions comp extlist
        when (not (null exts)) $
          die $ "The package " ++ display (packageId pkg_descr0)
             ++ " requires the following language extensions which are not "
             ++ "supported by " ++ display (compilerId comp) ++ ": "
             ++ intercalate ", " (map display exts)

        -- configured known/required programs & external build tools
        -- exclude build-tool deps on "internal" exes in the same package
        let requiredBuildTools =
              [ buildTool
              | let exeNames = map exeName (executables pkg_descr)
              , bi <- allBuildInfo pkg_descr
              , buildTool@(Dependency (PackageName toolName) reqVer) <- buildTools bi
              , let isInternal =
                        toolName `elem` exeNames
                        -- we assume all internal build-tools are
                        -- versioned with the package:
                     && packageVersion pkg_descr `withinRange` reqVer
              , not isInternal ]

        programsConfig'' <-
              configureAllKnownPrograms (lessVerbose verbosity) programsConfig'
          >>= configureRequiredPrograms verbosity requiredBuildTools

        (pkg_descr', programsConfig''') <-
          configurePkgconfigPackages verbosity pkg_descr programsConfig''

        split_objs <-
           if not (fromFlag $ configSplitObjs cfg)
                then return False
                else case flavor of
                            GHC | version >= Version [6,5] [] -> return True
                            _ -> do warn verbosity
                                         ("this compiler does not support " ++
                                          "--enable-split-objs; ignoring")
                                    return False

        -- The allPkgDeps contains all the package deps for the whole package
        -- but we need to select the subset for this specific component.
        -- we just take the subset for the package names this component
        -- needs. Note, this only works because we cannot yet depend on two
        -- versions of the same package.
        let configLib lib = configComponent (libBuildInfo lib)
            configExe exe = (exeName exe, configComponent (buildInfo exe))
            configTest test = (testName test,
                    configComponent(testBuildInfo test))
            configBenchmark bm = (benchmarkName bm,
                    configComponent(benchmarkBuildInfo bm))
            configComponent bi = ComponentLocalBuildInfo {
              componentPackageDeps =
                if newPackageDepsBehaviour pkg_descr'
                  then [ (installedPackageId pkg, packageId pkg)
                       | pkg <- selectSubset bi externalPkgDeps ]
                    ++ [ (inplacePackageId pkgid, pkgid)
                       | pkgid <- selectSubset bi internalPkgDeps ]
                  else [ (installedPackageId pkg, packageId pkg)
                       | pkg <- externalPkgDeps ]
            }
            selectSubset :: Package pkg => BuildInfo -> [pkg] -> [pkg]
            selectSubset bi pkgs =
                [ pkg | pkg <- pkgs, packageName pkg `elem` names ]
              where
                names = [ name | Dependency name _ <- targetBuildDepends bi ]

        -- Obtains the intrapackage dependencies for the given component
        let ipDeps component =
                 mapMaybe exeDepToComp (buildTools bi)
              ++ mapMaybe libDepToComp (targetBuildDepends bi)
              where
                bi = foldComponent libBuildInfo buildInfo testBuildInfo
                     benchmarkBuildInfo component
                exeDepToComp (Dependency (PackageName name) _) =
                  CExe `fmap` find ((==) name . exeName)
                                (executables pkg_descr')
                libDepToComp (Dependency pn _)
                  | pn `elem` map packageName internalPkgDeps =
                    CLib `fmap` library pkg_descr'
                libDepToComp _ = Nothing

        let sccs = (stronglyConnCompR . map lkup . vertices . transposeG) g
              where (g, lkup, _) = graphFromEdges
                                 $ allComponentsBy pkg_descr'
                                 $ \c -> (c, key c, map key (ipDeps c))
                    key          = foldComponent (const "library") exeName
                                   testName benchmarkName

        -- check for cycles in the dependency graph
        buildOrder <- forM sccs $ \scc -> case scc of
          AcyclicSCC (c,_,_) -> return (foldComponent (const CLibName)
                                                      (CExeName . exeName)
                                                      (CTestName . testName)
                                                      (CBenchName . benchmarkName)
                                                      c)
          CyclicSCC vs ->
            die $ "Found cycle in intrapackage dependency graph:\n  "
                ++ intercalate " depends on "
                     (map (\(_,k,_) -> "'" ++ k ++ "'") (vs ++ [head vs]))

        let lbi = LocalBuildInfo {
                    configFlags         = cfg,
                    extraConfigArgs     = [],  -- Currently configure does not
                                               -- take extra args, but if it
                                               -- did they would go here.
                    installDirTemplates = installDirs,
                    compiler            = comp,
                    buildDir            = buildDir',
                    scratchDir          = fromFlagOrDefault
                                            (distPref </> "scratch")
                                            (configScratchDir cfg),
                    libraryConfig       = configLib `fmap` library pkg_descr',
                    executableConfigs   = configExe `fmap` executables pkg_descr',
                    testSuiteConfigs    = configTest `fmap` testSuites pkg_descr',
                    benchmarkConfigs    = configBenchmark `fmap` benchmarks pkg_descr',
                    compBuildOrder      = buildOrder,
                    installedPkgs       = packageDependsIndex,
                    pkgDescrFile        = Nothing,
                    localPkgDescr       = pkg_descr',
                    withPrograms        = programsConfig''',
                    withVanillaLib      = fromFlag $ configVanillaLib cfg,
                    withProfLib         = fromFlag $ configProfLib cfg,
                    withSharedLib       = fromFlag $ configSharedLib cfg,
                    withDynExe          = fromFlag $ configDynExe cfg,
                    withProfExe         = fromFlag $ configProfExe cfg,
                    withOptimization    = fromFlag $ configOptimization cfg,
                    withGHCiLib         = fromFlag $ configGHCiLib cfg,
                    splitObjs           = split_objs,
                    stripExes           = fromFlag $ configStripExes cfg,
                    withPackageDB       = packageDbs,
                    progPrefix          = fromFlag $ configProgPrefix cfg,
                    progSuffix          = fromFlag $ configProgSuffix cfg
                  }

        let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
            relative = prefixRelativeInstallDirs (packageId pkg_descr) lbi

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
hackageUrl = "http://hackage.haskell.org/package/"

data ResolvedDependency = ExternalDependency Dependency InstalledPackageInfo
                        | InternalDependency Dependency PackageId -- should be a lib name

data FailedDependency = DependencyNotExists PackageName
                      | DependencyNoVersion Dependency

-- | Test for a package dependency and record the version we have installed.
selectDependency :: PackageIndex  -- ^ Internally defined packages
                 -> PackageIndex  -- ^ Installed packages
                 -> Dependency
                 -> Either FailedDependency ResolvedDependency
selectDependency internalIndex installedIndex
  dep@(Dependency pkgname vr) =
  -- If the dependency specification matches anything in the internal package
  -- index, then we prefer that match to anything in the second.
  -- For example:
  --
  -- Name: MyLibrary
  -- Version: 0.1
  -- Library
  --     ..
  -- Executable my-exec
  --     build-depends: MyLibrary
  --
  -- We want "build-depends: MyLibrary" always to match the internal library
  -- even if there is a newer installed library "MyLibrary-0.2".
  -- However, "build-depends: MyLibrary >= 0.2" should match the installed one.
  case PackageIndex.lookupPackageName internalIndex pkgname of
    [(_,[pkg])] | packageVersion pkg `withinRange` vr
           -> Right $ InternalDependency dep (packageId pkg)

    _      -> case PackageIndex.lookupDependency installedIndex dep of
      []   -> Left  $ DependencyNotExists pkgname
      pkgs -> Right $ ExternalDependency dep $
                -- by default we just pick the latest
                case last pkgs of
                  (_ver, instances) -> head instances -- the first preference

reportSelectedDependencies :: Verbosity
                           -> [ResolvedDependency] -> IO ()
reportSelectedDependencies verbosity deps =
  info verbosity $ unlines
    [ "Dependency " ++ display (simplifyDependency dep)
                    ++ ": using " ++ display pkgid
    | resolved <- deps
    , let (dep, pkgid) = case resolved of
            ExternalDependency dep' pkg'   -> (dep', packageId pkg')
            InternalDependency dep' pkgid' -> (dep', pkgid') ]

reportFailedDependencies :: [FailedDependency] -> IO ()
reportFailedDependencies []     = return ()
reportFailedDependencies failed =
    die (intercalate "\n\n" (map reportFailedDependency failed))

  where
    reportFailedDependency (DependencyNotExists pkgname) =
         "there is no version of " ++ display pkgname ++ " installed.\n"
      ++ "Perhaps you need to download and install it from\n"
      ++ hackageUrl ++ display pkgname ++ "?"

    reportFailedDependency (DependencyNoVersion dep) =
        "cannot satisfy dependency " ++ display (simplifyDependency dep) ++ "\n"

getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO PackageIndex
getInstalledPackages verbosity comp packageDBs progconf = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> GHC.getInstalledPackages verbosity packageDBs progconf
    Hugs->Hugs.getInstalledPackages verbosity packageDBs progconf
    JHC -> JHC.getInstalledPackages verbosity packageDBs progconf
    LHC -> LHC.getInstalledPackages verbosity packageDBs progconf
    NHC -> NHC.getInstalledPackages verbosity packageDBs progconf
    UHC -> UHC.getInstalledPackages verbosity comp packageDBs progconf
    flv -> die $ "don't know how to find the installed packages for "
              ++ display flv

-- | Currently the user interface specifies the package dbs to use with just a
-- single valued option, a 'PackageDB'. However internally we represent the
-- stack of 'PackageDB's explictly as a list. This function converts encodes
-- the package db stack implicit in a single packagedb.
--
implicitPackageDbStack :: Bool -> Maybe PackageDB -> PackageDBStack
implicitPackageDbStack userInstall maybePackageDB
  | userInstall = GlobalPackageDB : UserPackageDB : extra
  | otherwise   = GlobalPackageDB : extra
  where
    extra = case maybePackageDB of
      Just (SpecificPackageDB db) -> [SpecificPackageDB db]
      _                           -> []

newPackageDepsBehaviourMinVersion :: Version
newPackageDepsBehaviourMinVersion = Version { versionBranch = [1,7,1], versionTags = [] }

-- In older cabal versions, there was only one set of package dependencies for
-- the whole package. In this version, we can have separate dependencies per
-- target, but we only enable this behaviour if the minimum cabal version
-- specified is >= a certain minimum. Otherwise, for compatibility we use the
-- old behaviour.
newPackageDepsBehaviour :: PackageDescription -> Bool
newPackageDepsBehaviour pkg =
   specVersion pkg >= newPackageDepsBehaviourMinVersion

-- -----------------------------------------------------------------------------
-- Configuring program dependencies

configureRequiredPrograms :: Verbosity -> [Dependency] -> ProgramConfiguration -> IO ProgramConfiguration
configureRequiredPrograms verbosity deps conf =
  foldM (configureRequiredProgram verbosity) conf deps

configureRequiredProgram :: Verbosity -> ProgramConfiguration -> Dependency -> IO ProgramConfiguration
configureRequiredProgram verbosity conf (Dependency (PackageName progName) verRange) =
  case lookupKnownProgram progName conf of
    Nothing -> die ("Unknown build tool " ++ progName)
    Just prog
      -- requireProgramVersion always requires the program have a version
      -- but if the user says "build-depends: foo" ie no version constraint
      -- then we should not fail if we cannot discover the program version.
      | verRange == anyVersion -> do
          (_, conf') <- requireProgram verbosity prog conf
          return conf'
      | otherwise -> do
          (_, _, conf') <- requireProgramVersion verbosity prog verRange conf
          return conf'

-- -----------------------------------------------------------------------------
-- Configuring pkg-config package dependencies

configurePkgconfigPackages :: Verbosity -> PackageDescription
                           -> ProgramConfiguration
                           -> IO (PackageDescription, ProgramConfiguration)
configurePkgconfigPackages verbosity pkg_descr conf
  | null allpkgs = return (pkg_descr, conf)
  | otherwise    = do
    (_, _, conf') <- requireProgramVersion
                       (lessVerbose verbosity) pkgConfigProgram
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

    requirePkg dep@(Dependency (PackageName pkg) range) = do
      version <- pkgconfig ["--modversion", pkg]
                 `catchIO`   (\_ -> die notFound)
                 `catchExit` (\_ -> die notFound)
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
        depSatisfied v = "Dependency " ++ display dep
                      ++ ": using version " ++ display v

        versionRequirement
          | isAnyVersion range = ""
          | otherwise          = " version " ++ display range

    updateLibrary Nothing    = return Nothing
    updateLibrary (Just lib) = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (libBuildInfo lib))
      return $ Just lib { libBuildInfo = libBuildInfo lib `mappend` bi }

    updateExecutable exe = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (buildInfo exe))
      return exe { buildInfo = buildInfo exe `mappend` bi }

    pkgconfigBuildInfo :: [Dependency] -> IO BuildInfo
    pkgconfigBuildInfo []      = return mempty
    pkgconfigBuildInfo pkgdeps = do
      let pkgs = nub [ display pkg | Dependency pkg _ <- pkgdeps ]
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
                                       programsConfig
                                       (fromFlag (configVerbosity cfg))
  where
    programsConfig = userSpecifyArgss (configProgramArgs cfg)
                   . userSpecifyPaths (configProgramPaths cfg)
                   $ defaultProgramConfiguration

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> ProgramConfiguration -> Verbosity
               -> IO (Compiler, ProgramConfiguration)
configCompiler Nothing _ _ _ _ = die "Unknown compiler"
configCompiler (Just hcFlavor) hcPath hcPkg conf verbosity = do
  case hcFlavor of
      GHC  -> GHC.configure  verbosity hcPath hcPkg conf
      JHC  -> JHC.configure  verbosity hcPath hcPkg conf
      LHC  -> do (_,ghcConf) <- GHC.configure  verbosity Nothing hcPkg conf
                 LHC.configure  verbosity hcPath Nothing ghcConf
      Hugs -> Hugs.configure verbosity hcPath hcPkg conf
      NHC  -> NHC.configure  verbosity hcPath hcPkg conf
      UHC  -> UHC.configure  verbosity hcPath hcPkg conf
      _    -> die "Unknown compiler"


-- Try to build a test C program which includes every header and links every
-- lib. If that fails, try to narrow it down by preprocessing (only) and linking
-- with individual headers and libs.  If none is the obvious culprit then give a
-- generic error message.
-- TODO: produce a log file from the compiler errors, if any.
checkForeignDeps :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
checkForeignDeps pkg lbi verbosity = do
  ifBuildsWith allHeaders (commonCcArgs ++ makeLdArgs allLibs) -- I'm feeling lucky
           (return ())
           (do missingLibs <- findMissingLibs
               missingHdr  <- findOffendingHdr
               explainErrors missingHdr missingLibs)
      where
        allHeaders = collectField PD.includes
        allLibs    = collectField PD.extraLibs

        ifBuildsWith headers args success failure = do
            ok <- builds (makeProgram headers) args
            if ok then success else failure

        findOffendingHdr =
            ifBuildsWith allHeaders ccArgs
                         (return Nothing)
                         (go . tail . inits $ allHeaders)
            where
              go [] = return Nothing       -- cannot happen
              go (hdrs:hdrsInits) =
                    -- Try just preprocessing first
                    ifBuildsWith hdrs cppArgs
                      -- If that works, try compiling too
                      (ifBuildsWith hdrs ccArgs
                        (go hdrsInits)
                        (return . Just . Right . last $ hdrs))
                      (return . Just . Left . last $ hdrs)

              cppArgs = "-E":commonCppArgs -- preprocess only
              ccArgs  = "-c":commonCcArgs  -- don't try to link

        findMissingLibs = ifBuildsWith [] (makeLdArgs allLibs)
                                       (return [])
                                       (filterM (fmap not . libExists) allLibs)

        libExists lib = builds (makeProgram []) (makeLdArgs [lib])

        commonCppArgs = hcDefines (compiler lbi)
                     ++ [ "-I" ++ autogenModulesDir lbi ]
                     ++ [ "-I" ++ dir | dir <- collectField PD.includeDirs ]
                     ++ ["-I."]
                     ++ collectField PD.cppOptions
                     ++ collectField PD.ccOptions
                     ++ [ "-I" ++ dir
                        | dep <- deps
                        , dir <- Installed.includeDirs dep ]
                     ++ [ opt
                        | dep <- deps
                        , opt <- Installed.ccOptions dep ]

        commonCcArgs  = commonCppArgs
                     ++ collectField PD.ccOptions
                     ++ [ opt
                        | dep <- deps
                        , opt <- Installed.ccOptions dep ]

        commonLdArgs  = [ "-L" ++ dir | dir <- collectField PD.extraLibDirs ]
                     ++ collectField PD.ldOptions
                     ++ [ "-L" ++ dir
                        | dep <- deps
                        , dir <- Installed.libraryDirs dep ]
                     --TODO: do we also need dependent packages' ld options?
        makeLdArgs libs = [ "-l"++lib | lib <- libs ] ++ commonLdArgs

        makeProgram hdrs = unlines $
                           [ "#include \""  ++ hdr ++ "\"" | hdr <- hdrs ] ++
                           ["int main(int argc, char** argv) { return 0; }"]

        collectField f = concatMap f allBi
        allBi = allBuildInfo pkg
        deps = PackageIndex.topologicalOrder (installedPkgs lbi)

        builds program args = do
            tempDir <- getTemporaryDirectory
            withTempFile tempDir ".c" $ \cName cHnd ->
              withTempFile tempDir "" $ \oNname oHnd -> do
                hPutStrLn cHnd program
                hClose cHnd
                hClose oHnd
                _ <- rawSystemProgramStdoutConf verbosity
                  gccProgram (withPrograms lbi) (cName:"-o":oNname:args)
                return True
           `catchIO`   (\_ -> return False)
           `catchExit` (\_ -> return False)

        explainErrors Nothing [] = return () -- should be impossible!
        explainErrors hdr libs = die $ unlines $
             [ if plural
                 then "Missing dependencies on foreign libraries:"
                 else "Missing dependency on a foreign library:"
             | missing ]
          ++ case hdr of
               Just (Left h) -> ["* Missing (or bad) header file: " ++ h ]
               _             -> []
          ++ case libs of
               []    -> []
               [lib] -> ["* Missing C library: " ++ lib]
               _     -> ["* Missing C libraries: " ++ intercalate ", " libs]
          ++ [if plural then messagePlural else messageSingular | missing]
          ++ case hdr of
               Just (Left  _) -> [ headerCppMessage ]
               Just (Right h) -> [ (if missing then "* " else "")
                                   ++ "Bad header file: " ++ h
                                 , headerCcMessage ]
               _              -> []

          where
            plural  = length libs >= 2
            -- Is there something missing? (as opposed to broken)
            missing = not (null libs)
                   || case hdr of Just (Left _) -> True; _ -> False

        messageSingular =
             "This problem can usually be solved by installing the system "
          ++ "package that provides this library (you may need the "
          ++ "\"-dev\" version). If the library is already installed "
          ++ "but in a non-standard location then you can use the flags "
          ++ "--extra-include-dirs= and --extra-lib-dirs= to specify "
          ++ "where it is."
        messagePlural =
             "This problem can usually be solved by installing the system "
          ++ "packages that provide these libraries (you may need the "
          ++ "\"-dev\" versions). If the libraries are already installed "
          ++ "but in a non-standard location then you can use the flags "
          ++ "--extra-include-dirs= and --extra-lib-dirs= to specify "
          ++ "where they are."
        headerCppMessage =
             "If the header file does exist, it may contain errors that "
          ++ "are caught by the C compiler at the preprocessing stage. "
          ++ "In this case you can re-run configure with the verbosity "
          ++ "flag -v3 to see the error messages."
        headerCcMessage =
             "The header file contains a compile error. "
          ++ "You can re-run configure with the verbosity flag "
          ++ "-v3 to see the error messages from the C compiler."

        --FIXME: share this with the PreProcessor module
        hcDefines :: Compiler -> [String]
        hcDefines comp =
          case compilerFlavor comp of
            GHC  -> ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
            JHC  -> ["-D__JHC__=" ++ versionInt version]
            NHC  -> ["-D__NHC__=" ++ versionInt version]
            Hugs -> ["-D__HUGS__"]
            _    -> []
          where
            version = compilerVersion comp
                      -- TODO: move this into the compiler abstraction
            -- FIXME: this forces GHC's crazy 4.8.2 -> 408 convention on all
            -- the other compilers. Check if that's really what they want.
            versionInt :: Version -> String
            versionInt (Version { versionBranch = [] }) = "1"
            versionInt (Version { versionBranch = [n] }) = show n
            versionInt (Version { versionBranch = n1:n2:_ })
              = -- 6.8.x -> 608
                -- 6.10.x -> 610
                let s1 = show n1
                    s2 = show n2
                    middle = case s2 of
                             _ : _ : _ -> ""
                             _         -> "0"
                in s1 ++ middle ++ s2

-- | Output package check warnings and errors. Exit if any errors.
checkPackageProblems :: Verbosity
                     -> GenericPackageDescription
                     -> PackageDescription
                     -> IO ()
checkPackageProblems verbosity gpkg pkg = do
  ioChecks      <- checkPackageFiles pkg "."
  let pureChecks = checkPackage gpkg (Just pkg)
      errors   = [ e | PackageBuildImpossible e <- pureChecks ++ ioChecks ]
      warnings = [ w | PackageBuildWarning    w <- pureChecks ++ ioChecks ]
  if null errors
    then mapM_ (warn verbosity) warnings
    else do mapM_ (hPutStrLn stderr . ("Error: " ++)) errors
            exitWith (ExitFailure 1)
