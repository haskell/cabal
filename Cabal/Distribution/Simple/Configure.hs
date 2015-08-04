{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
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

module Distribution.Simple.Configure (configure,
                                      writePersistBuildConfig,
                                      getConfigStateFile,
                                      getPersistBuildConfig,
                                      checkPersistBuildConfigOutdated,
                                      tryGetPersistBuildConfig,
                                      maybeGetPersistBuildConfig,
                                      findDistPref, findDistPrefOrDefault,
                                      localBuildInfoFile,
                                      getInstalledPackages, getPackageDBContents,
                                      configCompiler, configCompilerAux,
                                      configCompilerEx, configCompilerAuxEx,
                                      ccLdOptionsBuildInfo,
                                      checkForeignDeps,
                                      interpretPackageDbFlags,
                                      ConfigStateFileError(..),
                                      tryGetConfigStateFile,
                                      platformDefines,
                                     )
    where

import Distribution.Compiler
    ( CompilerId(..) )
import Distribution.Utils.NubList
import Distribution.Simple.Compiler
    ( CompilerFlavor(..), Compiler(..), compilerFlavor, compilerVersion
    , compilerInfo, ProfDetailLevel(..), knownProfDetailLevels
    , showCompilerId, unsupportedLanguages, unsupportedExtensions
    , PackageDB(..), PackageDBStack, reexportedModulesSupported
    , packageKeySupported, renamingPackageFlagsSupported )
import Distribution.Simple.PreProcess ( platformDefines, knownSuffixHandlers )
import Distribution.Package
    ( PackageName(PackageName), PackageIdentifier(..), PackageId
    , packageName, packageVersion, Package(..)
    , Dependency(Dependency), simplifyDependency
    , InstalledPackageId(..), thisPackageVersion
    , mkPackageKey, packageKeyLibraryName )
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.InstalledPackageInfo (InstalledPackageInfo, emptyInstalledPackageInfo)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.PackageDescription as PD
    ( PackageDescription(..), specVersion, GenericPackageDescription(..)
    , Library(..), hasLibs, Executable(..), BuildInfo(..), allExtensions
    , HookedBuildInfo, updatePackageDescription, allBuildInfo
    , Flag(flagName), FlagName(..), TestSuite(..), Benchmark(..)
    , ModuleReexport(..) , defaultRenaming, FlagAssignment )
import Distribution.ModuleName
    ( ModuleName )
import Distribution.PackageDescription.Configuration
    ( finalizePackageDescription, mapTreeData )
import Distribution.PackageDescription.Check
    ( PackageCheck(..), checkPackage, checkPackageFiles )
import Distribution.Simple.Program
    ( Program(..), ProgramLocation(..), ConfiguredProgram(..)
    , ProgramConfiguration, defaultProgramConfiguration
    , ProgramSearchPathEntry(..), getProgramSearchPath, setProgramSearchPath
    , configureAllKnownPrograms, knownPrograms, lookupKnownProgram
    , userSpecifyArgss, userSpecifyPaths
    , lookupProgram, requireProgram, requireProgramVersion
    , pkgConfigProgram, gccProgram, rawSystemProgramStdoutConf )
import Distribution.Simple.Setup as Setup
    ( ConfigFlags(..), CopyDest(..), Flag(..), defaultDistPref
    , fromFlag, fromFlagOrDefault, flagToMaybe, toFlag )
import Distribution.Simple.InstallDirs
    ( InstallDirs(..), defaultInstallDirs, combineInstallDirs )
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo(..), Component(..), ComponentLocalBuildInfo(..)
    , absoluteInstallDirs, prefixRelativeInstallDirs, inplacePackageId
    , ComponentName(..), showComponentName, pkgEnabledComponents
    , componentBuildInfo, componentName, checkComponentsCyclic )
import Distribution.Simple.SrcDist ( listPackageSources )
import Distribution.Simple.BuildPaths
    ( autogenModulesDir )
import Distribution.Simple.Utils
    ( die, warn, info, setupMessage
    , createDirectoryIfMissingVerbose, moreRecentFile
    , intercalate, cabalVersion
    , writeFileAtomic
    , withTempFile )
import Distribution.System
    ( OS(..), buildOS, Platform (..), buildPlatform )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion, withinRange, isAnyVersion )
import Distribution.Verbosity
    ( Verbosity, lessVerbose, silent )

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite

-- Prefer the more generic Data.Traversable.mapM to Prelude.mapM
import Prelude hiding ( mapM )
import Control.Exception
    ( ErrorCall(..), Exception, evaluate, throw, throwIO, try )
import Control.Monad
    ( liftM, when, unless, foldM, filterM )
import Distribution.Compat.Binary ( decodeOrFailIO, encode )
import GHC.Fingerprint ( Fingerprint(..), fingerprintString
#if __GLASGOW_HASKELL__ >= 710
                       , getFileHash
#endif
                       )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.List
    ( (\\), nub, partition, isPrefixOf, inits, stripPrefix, sort )
import Data.Maybe
    ( isNothing, catMaybes, fromMaybe, isJust )
import Data.Either
    ( partitionEithers )
import qualified Data.Set as Set
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
    ( Monoid(..) )
#endif
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable
    ( mapM )
import Data.Typeable
import Data.Char ( chr )
import Numeric ( showIntAtBase, showHex )
import Data.Bits ( shift )
import System.Directory
    ( doesFileExist, createDirectoryIfMissing, getTemporaryDirectory )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, hClose )
import Distribution.Text
    ( Text(disp), display, simpleParse )
import Text.PrettyPrint
    ( render, (<>), ($+$), char, text, comma
    , quotes, punctuate, nest, sep, hsep )
import Distribution.Compat.Environment ( lookupEnv )
import Distribution.Compat.Exception ( catchExit, catchIO )

-- | The errors that can be thrown when reading the @setup-config@ file.
data ConfigStateFileError
    = ConfigStateFileNoHeader -- ^ No header found.
    | ConfigStateFileBadHeader -- ^ Incorrect header.
    | ConfigStateFileNoParse -- ^ Cannot parse file contents.
    | ConfigStateFileMissing -- ^ No file!
    | ConfigStateFileBadVersion PackageIdentifier PackageIdentifier (Either ConfigStateFileError LocalBuildInfo) -- ^ Mismatched version.
  deriving (Typeable)

instance Show ConfigStateFileError where
    show ConfigStateFileNoHeader =
        "Saved package config file header is missing. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileBadHeader =
        "Saved package config file header is corrupt. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileNoParse =
        "Saved package config file body is corrupt. "
        ++ "Try re-running the 'configure' command."
    show ConfigStateFileMissing = "Run the 'configure' command first."
    show (ConfigStateFileBadVersion oldCabal oldCompiler _) =
        "You need to re-run the 'configure' command. "
        ++ "The version of Cabal being used has changed (was "
        ++ display oldCabal ++ ", now "
        ++ display currentCabalId ++ ")."
        ++ badCompiler
      where
        badCompiler
          | oldCompiler == currentCompilerId = ""
          | otherwise =
              " Additionally the compiler is different (was "
              ++ display oldCompiler ++ ", now "
              ++ display currentCompilerId
              ++ ") which is probably the cause of the problem."

instance Exception ConfigStateFileError

-- | Read the 'localBuildInfoFile'.  Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                   -> IO LocalBuildInfo
getConfigStateFile filename = do
    exists <- doesFileExist filename
    unless exists $ throwIO ConfigStateFileMissing
    -- Read the config file into a strict ByteString to avoid problems with
    -- lazy I/O, then convert to lazy because the binary package needs that.
    contents <- BS.readFile filename
    let (header, body) = BLC8.span (/='\n') (BLC8.fromChunks [contents])

    headerParseResult <- try $ evaluate $ parseHeader header
    let (cabalId, compId) =
            case headerParseResult of
              Left (ErrorCall _) -> throw ConfigStateFileBadHeader
              Right x -> x

    let getStoredValue = do
          result <- decodeOrFailIO (BLC8.tail body)
          case result of
            Left _ -> throw ConfigStateFileNoParse
            Right x -> return x
        deferErrorIfBadVersion act
          | cabalId /= currentCabalId = do
              eResult <- try act
              throw $ ConfigStateFileBadVersion cabalId compId eResult
          | otherwise = act
    deferErrorIfBadVersion getStoredValue

-- | Read the 'localBuildInfoFile', returning either an error or the local build info.
tryGetConfigStateFile :: FilePath -- ^ The file path of the @setup-config@ file.
                      -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetConfigStateFile = try . getConfigStateFile

-- | Try to read the 'localBuildInfoFile'.
tryGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                         -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetPersistBuildConfig = try . getPersistBuildConfig

-- | Read the 'localBuildInfoFile'. Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                      -> IO LocalBuildInfo
getPersistBuildConfig = getConfigStateFile . localBuildInfoFile

-- | Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                           -> IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig =
    liftM (either (const Nothing) Just) . tryGetPersistBuildConfig

-- | After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: FilePath -- ^ The @dist@ directory path.
                        -> LocalBuildInfo -- ^ The 'LocalBuildInfo' to write.
                        -> IO ()
writePersistBuildConfig distPref lbi = do
    createDirectoryIfMissing False distPref
    writeFileAtomic (localBuildInfoFile distPref) $
      BLC8.unlines [showHeader pkgId, encode lbi]
  where
    pkgId = packageId $ localPkgDescr lbi

-- | Identifier of the current Cabal package.
currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier (PackageName "Cabal") cabalVersion

-- | Identifier of the current compiler package.
currentCompilerId :: PackageIdentifier
currentCompilerId = PackageIdentifier (PackageName System.Info.compilerName)
                                      System.Info.compilerVersion

-- | Parse the @setup-config@ file header, returning the package identifiers 
-- for Cabal and the compiler.
parseHeader :: ByteString -- ^ The file contents.
            -> (PackageIdentifier, PackageIdentifier)
parseHeader header = case BLC8.words header of
  ["Saved", "package", "config", "for", pkgId, "written", "by", cabalId, "using", compId] ->
      fromMaybe (throw ConfigStateFileBadHeader) $ do
          _ <- simpleParse (BLC8.unpack pkgId) :: Maybe PackageIdentifier
          cabalId' <- simpleParse (BLC8.unpack cabalId)
          compId' <- simpleParse (BLC8.unpack compId)
          return (cabalId', compId')
  _ -> throw ConfigStateFileNoHeader

-- | Generate the @setup-config@ file header.
showHeader :: PackageIdentifier -- ^ The processed package.
            -> ByteString
showHeader pkgId = BLC8.unwords
    [ "Saved", "package", "config", "for"
    , BLC8.pack $ display pkgId
    , "written", "by"
    , BLC8.pack $ display currentCabalId
    , "using"
    , BLC8.pack $ display currentCompilerId
    ]

-- | Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfigOutdated :: FilePath -> FilePath -> IO Bool
checkPersistBuildConfigOutdated distPref pkg_descr_file = do
  pkg_descr_file `moreRecentFile` (localBuildInfoFile distPref)

-- | Get the path of @dist\/setup-config@.
localBuildInfoFile :: FilePath -- ^ The @dist@ directory path.
                    -> FilePath
localBuildInfoFile distPref = distPref </> "setup-config"

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken from
-- (in order of highest to lowest preference) the override prefix, the \"CABAL_BUILDDIR\"
-- environment variable, or the default prefix.
findDistPref :: FilePath  -- ^ default \"dist\" prefix
             -> Setup.Flag FilePath  -- ^ override \"dist\" prefix
             -> IO FilePath
findDistPref defDistPref overrideDistPref = do
    envDistPref <- liftM parseEnvDistPref (lookupEnv "CABAL_BUILDDIR")
    return $ fromFlagOrDefault defDistPref (mappend envDistPref overrideDistPref)
  where
    parseEnvDistPref env =
      case env of
        Just distPref | not (null distPref) -> toFlag distPref
        _ -> NoFlag

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken from
-- (in order of highest to lowest preference) the override prefix, the \"CABAL_BUILDDIR\"
-- environment variable, or 'defaultDistPref' is used. Call this function to resolve a
-- @*DistPref@ flag whenever it is not known to be set. (The @*DistPref@ flags are always
-- set to a definite value before invoking 'UserHooks'.)
findDistPrefOrDefault :: Setup.Flag FilePath  -- ^ override \"dist\" prefix
                      -> IO FilePath
findDistPrefOrDefault = findDistPref defaultDistPref

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg
  = do  let distPref = fromFlag (configDistPref cfg)
            buildDir' = distPref </> "build"

        setupMessage verbosity "Configuring" (packageId pkg_descr0)

        unless (configProfExe cfg == NoFlag) $ do
          let enable | fromFlag (configProfExe cfg) = "enable"
                     | otherwise = "disable"
          warn verbosity
            ("The flag --" ++ enable ++ "-executable-profiling is deprecated. "
             ++ "Please use --" ++ enable ++ "-profiling instead.")

        unless (configLibCoverage cfg == NoFlag) $ do
          let enable | fromFlag (configLibCoverage cfg) = "enable"
                     | otherwise = "disable"
          warn verbosity
            ("The flag --" ++ enable ++ "-library-coverage is deprecated. "
             ++ "Please use --" ++ enable ++ "-coverage instead.")

        createDirectoryIfMissingVerbose (lessVerbose verbosity) True distPref

        let programsConfig = mkProgramsConfig cfg (configPrograms cfg)
            userInstall    = fromFlag (configUserInstall cfg)
            packageDbs     = interpretPackageDbFlags userInstall
                             (configPackageDBs cfg)

        -- detect compiler
        (comp, compPlatform, programsConfig') <- configCompilerEx
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
                Installed.installedPackageId =
                   InstalledPackageId $ display $ pid,
                Installed.sourcePackageId = pid
              }
            internalPackageSet = PackageIndex.fromList [internalPackage]
        installedPackageSet <- getInstalledPackages (lessVerbose verbosity) comp
                                      packageDbs programsConfig'

        (allConstraints, requiredDepsMap) <- either die return $
          combinedConstraints (configConstraints cfg)
                              (configDependencies cfg)
                              installedPackageSet

        let exactConf = fromFlagOrDefault False (configExactConfiguration cfg)
            -- Constraint test function for the solver
            dependencySatisfiable d@(Dependency depName verRange)
              | exactConf =
                -- When we're given '--exact-configuration', we assume that all
                -- dependencies and flags are exactly specified on the command
                -- line. Thus we only consult the 'requiredDepsMap'. Note that
                -- we're not doing the version range check, so if there's some
                -- dependency that wasn't specified on the command line,
                -- 'finalizePackageDescription' will fail.
                --
                -- TODO: mention '--exact-configuration' in the error message
                -- when this fails?
                (depName `Map.member` requiredDepsMap) || isInternalDep

              | otherwise =
                -- Normal operation: just look up dependency in the package
                -- index.
                not . null . PackageIndex.lookupDependency pkgs' $ d
              where
                pkgs' = PackageIndex.insert internalPackage installedPackageSet
                isInternalDep = pkgName pid == depName
                                && pkgVersion pid `withinRange` verRange
            enableTest t = t { testEnabled = fromFlag (configTests cfg) }
            flaggedTests = map (\(n, t) -> (n, mapTreeData enableTest t))
                               (condTestSuites pkg_descr0)
            enableBenchmark bm = bm { benchmarkEnabled =
                                         fromFlag (configBenchmarks cfg) }
            flaggedBenchmarks = map (\(n, bm) ->
                                      (n, mapTreeData enableBenchmark bm))
                               (condBenchmarks pkg_descr0)
            pkg_descr0'' = pkg_descr0 { condTestSuites = flaggedTests
                                      , condBenchmarks = flaggedBenchmarks }

        (pkg_descr0', flags) <-
                case finalizePackageDescription
                       (configConfigurationsFlags cfg)
                       dependencySatisfiable
                       compPlatform
                       (compilerInfo comp)
                       allConstraints
                       pkg_descr0''
                of Right r -> return r
                   Left missing ->
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma
                                    . map (disp . simplifyDependency)
                                    $ missing)

        -- Sanity check: if '--exact-configuration' was given, ensure that the
        -- complete flag assignment was specified on the command line.
        when exactConf $ do
          let cmdlineFlags = map fst (configConfigurationsFlags cfg)
              allFlags     = map flagName . genPackageFlags $ pkg_descr0
              diffFlags    = allFlags \\ cmdlineFlags
          when (not . null $ diffFlags) $
            die $ "'--exact-conf' was given, "
            ++ "but the following flags were not specified: "
            ++ intercalate ", " (map show diffFlags)

        -- add extra include/lib dirs as specified in cfg
        -- we do it here so that those get checked too
        let pkg_descr = addExtraIncludeLibDirs pkg_descr0'

        unless (renamingPackageFlagsSupported comp ||
                    and [ rn == defaultRenaming
                        | bi <- allBuildInfo pkg_descr
                        , rn <- Map.elems (targetBuildRenaming bi)]) $
            die $ "Your compiler does not support thinning and renaming on "
               ++ "package flags.  To use this feature you probably must use "
               ++ "GHC 7.9 or later."

        when (not (null flags)) $
          info verbosity $ "Flags chosen: "
                        ++ intercalate ", " [ name ++ "=" ++ display value
                                            | (FlagName name, value) <- flags ]

        when (maybe False (not.null.PD.reexportedModules) (PD.library pkg_descr)
              && not (reexportedModulesSupported comp)) $ do
            die $ "Your compiler does not support module re-exports. To use "
               ++ "this feature you probably must use GHC 7.9 or later."

        checkPackageProblems verbosity pkg_descr0
          (updatePackageDescription pbi pkg_descr)

        -- Handle hole instantiation
        (holeDeps, hole_insts) <- configureInstantiateWith pkg_descr cfg installedPackageSet

        let selectDependencies :: [Dependency] ->
                                  ([FailedDependency], [ResolvedDependency])
            selectDependencies =
                (\xs -> ([ x | Left x <- xs ], [ x | Right x <- xs ]))
              . map (selectDependency internalPackageSet installedPackageSet
                                      requiredDepsMap)

            (failedDeps, allPkgDeps) =
              selectDependencies (buildDepends pkg_descr)

            internalPkgDeps = [ pkgid
                              | InternalDependency _ pkgid <- allPkgDeps ]
            externalPkgDeps = [ pkg
                              | ExternalDependency _ pkg   <- allPkgDeps ]

        when (not (null internalPkgDeps)
              && not (newPackageDepsBehaviour pkg_descr)) $
            die $ "The field 'build-depends: "
               ++ intercalate ", " (map (display . packageName) internalPkgDeps)
               ++ "' refers to a library which is defined within the same "
               ++ "package. To use this feature the package must specify at "
               ++ "least 'cabal-version: >= 1.8'."

        reportFailedDependencies failedDeps
        reportSelectedDependencies verbosity allPkgDeps

        let installDeps = Map.elems
                        . Map.fromList
                        . map (\v -> (Installed.installedPackageId v, v))
                        $ externalPkgDeps ++ holeDeps

        packageDependsIndex <-
          case PackageIndex.dependencyClosure installedPackageSet
                  (map Installed.installedPackageId installDeps) of
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
                Installed.installedPackageId =
                   InstalledPackageId (display (packageId pkg_descr)),
                Installed.sourcePackageId = packageId pkg_descr,
                Installed.depends =
                  map Installed.installedPackageId installDeps
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
        let langlist = nub $ catMaybes $ map defaultLanguage
                       (allBuildInfo pkg_descr)
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
              , buildTool@(Dependency (PackageName toolName) reqVer)
                <- buildTools bi
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

        -- internal component graph
        buildComponents <-
          case mkComponentsGraph pkg_descr internalPkgDeps of
            Left  componentCycle -> reportComponentCycle componentCycle
            Right components     ->
              mkComponentsLocalBuildInfo comp packageDependsIndex pkg_descr
                                         internalPkgDeps externalPkgDeps holeDeps
                                         (Map.fromList hole_insts)
                                         components (configConfigurationsFlags cfg)

        split_objs <-
           if not (fromFlag $ configSplitObjs cfg)
                then return False
                else case flavor of
                            GHC | version >= Version [6,5] [] -> return True
                            GHCJS                             -> return True
                            _ -> do warn verbosity
                                         ("this compiler does not support " ++
                                          "--enable-split-objs; ignoring")
                                    return False

        let ghciLibByDefault =
              case compilerId comp of
                CompilerId GHC _ ->
                  -- If ghc is non-dynamic, then ghci needs object files,
                  -- so we build one by default.
                  --
                  -- Technically, archive files should be sufficient for ghci,
                  -- but because of GHC bug #8942, it has never been safe to
                  -- rely on them. By the time that bug was fixed, ghci had
                  -- been changed to read shared libraries instead of archive
                  -- files (see next code block).
                  not (GHC.isDynamic comp)
                CompilerId GHCJS _ ->
                  not (GHCJS.isDynamic comp)
                _ -> False

        let sharedLibsByDefault
              | fromFlag (configDynExe cfg) =
                  -- build a shared library if dynamically-linked
                  -- executables are requested
                  True
              | otherwise = case compilerId comp of
                CompilerId GHC _ ->
                  -- if ghc is dynamic, then ghci needs a shared
                  -- library, so we build one by default.
                  GHC.isDynamic comp
                CompilerId GHCJS _ ->
                  GHCJS.isDynamic comp
                _ -> False
            withSharedLib_ =
                -- build shared libraries if required by GHC or by the
                -- executable linking mode, but allow the user to force
                -- building only static library archives with
                -- --disable-shared.
                fromFlagOrDefault sharedLibsByDefault $ configSharedLib cfg
            withDynExe_ = fromFlag $ configDynExe cfg
        when (withDynExe_ && not withSharedLib_) $ warn verbosity $
               "Executables will use dynamic linking, but a shared library "
            ++ "is not being built. Linking will fail if any executables "
            ++ "depend on the library."

        -- The --profiling flag sets the default for both libs and exes,
        -- but can be overidden by --library-profiling, or the old deprecated
        -- --executable-profiling flag.
        let profEnabledLibOnly = configProfLib cfg
            profEnabledBoth    = fromFlagOrDefault False (configProf cfg)
            profEnabledLib = fromFlagOrDefault profEnabledBoth profEnabledLibOnly
            profEnabledExe = fromFlagOrDefault profEnabledBoth (configProfExe cfg)

        -- The --profiling-detail and --library-profiling-detail flags behave
        -- similarly
        profDetailLibOnly <- checkProfDetail (configProfLibDetail cfg)
        profDetailBoth    <- liftM (fromFlagOrDefault ProfDetailDefault)
                                   (checkProfDetail (configProfDetail cfg))
        let profDetailLib = fromFlagOrDefault profDetailBoth profDetailLibOnly
            profDetailExe = profDetailBoth

        when (profEnabledExe && not profEnabledLib) $
          warn verbosity $
               "Executables will be built with profiling, but library "
            ++ "profiling is disabled. Linking will fail if any executables "
            ++ "depend on the library."

        let configCoverage_ =
              mappend (configCoverage cfg) (configLibCoverage cfg)

            cfg' = cfg { configCoverage = configCoverage_ }

        reloc <-
           if not (fromFlag $ configRelocatable cfg)
                then return False
                else return True

        let lbi = LocalBuildInfo {
                    configFlags         = cfg',
                    extraConfigArgs     = [],  -- Currently configure does not
                                               -- take extra args, but if it
                                               -- did they would go here.
                    installDirTemplates = installDirs,
                    compiler            = comp,
                    hostPlatform        = compPlatform,
                    buildDir            = buildDir',
                    componentsConfigs   = buildComponents,
                    installedPkgs       = packageDependsIndex,
                    pkgDescrFile        = Nothing,
                    localPkgDescr       = pkg_descr',
                    instantiatedWith    = hole_insts,
                    withPrograms        = programsConfig''',
                    withVanillaLib      = fromFlag $ configVanillaLib cfg,
                    withProfLib         = profEnabledLib,
                    withSharedLib       = withSharedLib_,
                    withDynExe          = withDynExe_,
                    withProfExe         = profEnabledExe,
                    withProfLibDetail   = profDetailLib,
                    withProfExeDetail   = profDetailExe,
                    withOptimization    = fromFlag $ configOptimization cfg,
                    withDebugInfo       = fromFlag $ configDebugInfo cfg,
                    withGHCiLib         = fromFlagOrDefault ghciLibByDefault $
                                          configGHCiLib cfg,
                    splitObjs           = split_objs,
                    stripExes           = fromFlag $ configStripExes cfg,
                    stripLibs           = fromFlag $ configStripLibs cfg,
                    withPackageDB       = packageDbs,
                    progPrefix          = fromFlag $ configProgPrefix cfg,
                    progSuffix          = fromFlag $ configProgSuffix cfg,
                    relocatable         = reloc
                  }

        when reloc (checkRelocatable verbosity pkg_descr lbi)

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
        dirinfo "Configuration files" (sysconfdir dirs) (sysconfdir relative)

        sequence_ [ reportProgram verbosity prog configuredProg
                  | (prog, configuredProg) <- knownPrograms programsConfig''' ]

        return lbi

    where
      verbosity = fromFlag (configVerbosity cfg)

      addExtraIncludeLibDirs pkg_descr =
          let extraBi = mempty { extraLibDirs = configExtraLibDirs cfg
                               , PD.includeDirs = configExtraIncludeDirs cfg}
              modifyLib l        = l{ libBuildInfo = libBuildInfo l
                                                     `mappend` extraBi }
              modifyExecutable e = e{ buildInfo    = buildInfo e
                                                     `mappend` extraBi}
          in pkg_descr{ library     = modifyLib        `fmap` library pkg_descr
                      , executables = modifyExecutable  `map`
                                      executables pkg_descr}

      checkProfDetail (Flag (ProfDetailOther other)) = do
        warn verbosity $
             "Unknown profiling detail level '" ++ other
          ++ "', using default.\n"
          ++ "The profiling detail levels are: " ++ intercalate ", "
             [ name | (name, _, _) <- knownProfDetailLevels ]
        return (Flag ProfDetailDefault)
      checkProfDetail other = return other

mkProgramsConfig :: ConfigFlags -> ProgramConfiguration -> ProgramConfiguration
mkProgramsConfig cfg initialProgramsConfig = programsConfig
  where
    programsConfig = userSpecifyArgss (configProgramArgs cfg)
                   . userSpecifyPaths (configProgramPaths cfg)
                   . setProgramSearchPath searchpath
                   $ initialProgramsConfig
    searchpath     = getProgramSearchPath (initialProgramsConfig)
                  ++ map ProgramSearchPathDir (fromNubList $ configProgramPathExtra cfg)

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
                        | InternalDependency Dependency PackageId -- should be a
                                                                      -- lib name

data FailedDependency = DependencyNotExists PackageName
                      | DependencyNoVersion Dependency

-- | Test for a package dependency and record the version we have installed.
selectDependency :: InstalledPackageIndex  -- ^ Internally defined packages
                 -> InstalledPackageIndex  -- ^ Installed packages
                 -> Map PackageName InstalledPackageInfo
                    -- ^ Packages for which we have been given specific deps to use
                 -> Dependency
                 -> Either FailedDependency ResolvedDependency
selectDependency internalIndex installedIndex requiredDepsMap
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

    _      -> case Map.lookup pkgname requiredDepsMap of
      -- If we know the exact pkg to use, then use it.
      Just pkginstance -> Right (ExternalDependency dep pkginstance)
      -- Otherwise we just pick an arbitrary instance of the latest version.
      Nothing -> case PackageIndex.lookupDependency installedIndex dep of
        []   -> Left  $ DependencyNotExists pkgname
        pkgs -> Right $ ExternalDependency dep $
                case last pkgs of
                  (_ver, pkginstances) -> head pkginstances

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

-- | List all installed packages in the given package databases.
getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -- ^ The stack of package databases.
                     -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packageDBs progconf = do
  when (null packageDBs) $
    die $ "No package databases have been specified. If you use "
       ++ "--package-db=clear, you must follow it with --package-db= "
       ++ "with 'global', 'user' or a specific file."

  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC   -> GHC.getInstalledPackages verbosity comp packageDBs progconf
    GHCJS -> GHCJS.getInstalledPackages verbosity packageDBs progconf
    JHC   -> JHC.getInstalledPackages verbosity packageDBs progconf
    LHC   -> LHC.getInstalledPackages verbosity packageDBs progconf
    UHC   -> UHC.getInstalledPackages verbosity comp packageDBs progconf
    HaskellSuite {} ->
      HaskellSuite.getInstalledPackages verbosity packageDBs progconf
    flv -> die $ "don't know how to find the installed packages for "
              ++ display flv

-- | Like 'getInstalledPackages', but for a single package DB.
getPackageDBContents :: Verbosity -> Compiler
                     -> PackageDB -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getPackageDBContents verbosity comp packageDB progconf = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> GHC.getPackageDBContents verbosity packageDB progconf
    GHCJS -> GHCJS.getPackageDBContents verbosity packageDB progconf
    -- For other compilers, try to fall back on 'getInstalledPackages'.
    _   -> getInstalledPackages verbosity comp [packageDB] progconf


-- | The user interface specifies the package dbs to use with a combination of
-- @--global@, @--user@ and @--package-db=global|user|clear|$file@.
-- This function combines the global/user flag and interprets the package-db
-- flag into a single package db stack.
--
interpretPackageDbFlags :: Bool -> [Maybe PackageDB] -> PackageDBStack
interpretPackageDbFlags userInstall specificDBs =
    extra initialStack specificDBs
  where
    initialStack | userInstall = [GlobalPackageDB, UserPackageDB]
                 | otherwise   = [GlobalPackageDB]

    extra dbs' []            = dbs'
    extra _    (Nothing:dbs) = extra []             dbs
    extra dbs' (Just db:dbs) = extra (dbs' ++ [db]) dbs

newPackageDepsBehaviourMinVersion :: Version
newPackageDepsBehaviourMinVersion = Version [1,7,1] []

-- In older cabal versions, there was only one set of package dependencies for
-- the whole package. In this version, we can have separate dependencies per
-- target, but we only enable this behaviour if the minimum cabal version
-- specified is >= a certain minimum. Otherwise, for compatibility we use the
-- old behaviour.
newPackageDepsBehaviour :: PackageDescription -> Bool
newPackageDepsBehaviour pkg =
   specVersion pkg >= newPackageDepsBehaviourMinVersion

-- We are given both --constraint="foo < 2.0" style constraints and also
-- specific packages to pick via --dependency="foo=foo-2.0-177d5cdf20962d0581".
--
-- When finalising the package we have to take into account the specific
-- installed deps we've been given, and the finalise function expects
-- constraints, so we have to translate these deps into version constraints.
--
-- But after finalising we then have to make sure we pick the right specific
-- deps in the end. So we still need to remember which installed packages to
-- pick.
combinedConstraints :: [Dependency] ->
                       [(PackageName, InstalledPackageId)] ->
                       InstalledPackageIndex ->
                       Either String ([Dependency],
                                      Map PackageName InstalledPackageInfo)
combinedConstraints constraints dependencies installedPackages = do

    when (not (null badInstalledPackageIds)) $
      Left $ render $ text "The following package dependencies were requested"
         $+$ nest 4 (dispDependencies badInstalledPackageIds)
         $+$ text "however the given installed package instance does not exist."

    when (not (null badNames)) $
      Left $ render $ text "The following package dependencies were requested"
         $+$ nest 4 (dispDependencies badNames)
         $+$ text "however the installed package's name does not match the name given."

    --TODO: we don't check that all dependencies are used!

    return (allConstraints, idConstraintMap)

  where
    allConstraints :: [Dependency]
    allConstraints = constraints
                  ++ [ thisPackageVersion (packageId pkg)
                     | (_, _, Just pkg) <- dependenciesPkgInfo ]

    idConstraintMap :: Map PackageName InstalledPackageInfo
    idConstraintMap = Map.fromList
                        [ (packageName pkg, pkg)
                        | (_, _, Just pkg) <- dependenciesPkgInfo ]

    -- The dependencies along with the installed package info, if it exists
    dependenciesPkgInfo :: [(PackageName, InstalledPackageId,
                             Maybe InstalledPackageInfo)]
    dependenciesPkgInfo =
      [ (pkgname, ipkgid, mpkg)
      | (pkgname, ipkgid) <- dependencies
      , let mpkg = PackageIndex.lookupInstalledPackageId
                     installedPackages ipkgid
      ]

    -- If we looked up a package specified by an installed package id
    -- (i.e. someone has written a hash) and didn't find it then it's
    -- an error.
    badInstalledPackageIds =
      [ (pkgname, ipkgid)
      | (pkgname, ipkgid, Nothing) <- dependenciesPkgInfo ]

    -- If someone has written e.g.
    -- --dependency="foo=MyOtherLib-1.0-07...5bf30" then they have
    -- probably made a mistake.
    badNames =
      [ (requestedPkgName, ipkgid)
      | (requestedPkgName, ipkgid, Just pkg) <- dependenciesPkgInfo
      , let foundPkgName = packageName pkg
      , requestedPkgName /= foundPkgName ]

    dispDependencies deps =
      hsep [    text "--dependency="
             <> quotes (disp pkgname <> char '=' <> disp ipkgid)
           | (pkgname, ipkgid) <- deps ]

-- -----------------------------------------------------------------------------
-- Configuring hole instantiation

configureInstantiateWith :: PackageDescription
                         -> ConfigFlags
                         -> InstalledPackageIndex -- ^ installed packages
                         -> IO ([InstalledPackageInfo],
                                [(ModuleName, (InstalledPackageInfo, ModuleName))])
configureInstantiateWith pkg_descr cfg installedPackageSet = do
        -- Holes: First, check and make sure the provided instantiation covers
        -- all the holes we know about.  Indefinite package installation is
        -- not handled at all at this point.
        -- NB: We union together /all/ of the requirements when calculating
        -- the package key.
        -- NB: For now, we assume that dependencies don't contribute signatures.
        -- This will be handled by cabal-install; as far as ./Setup is
        -- concerned, the most important thing is to be provided correctly
        -- built dependencies.
        let signatures =
              maybe [] (\lib -> requiredSignatures lib ++ exposedSignatures lib)
                (PD.library pkg_descr)
            signatureSet = Set.fromList signatures
            instantiateMap = Map.fromList (configInstantiateWith cfg)
            missing_impls = filter (not . flip Map.member instantiateMap) signatures
            hole_insts0 = filter (\(k,_) -> Set.member k signatureSet) (configInstantiateWith cfg)

        when (not (null missing_impls)) $
          die $ "Missing signature implementations for these modules: "
            ++ intercalate ", " (map display missing_impls)

        -- Holes: Next, we need to make sure we have packages to actually
        -- provide the implementations we're talking about.  This is on top
        -- of the normal dependency resolution process.
        -- TODO: internal dependencies (e.g. the test package depending on the
        -- main library) is not currently supported
        let selectHoleDependency (k,(i,m)) =
              case PackageIndex.lookupInstalledPackageId installedPackageSet i of
                Just pkginst -> Right (k,(pkginst, m))
                Nothing -> Left i
            (failed_hmap, hole_insts) = partitionEithers (map selectHoleDependency hole_insts0)
            holeDeps = map (fst.snd) hole_insts -- could have dups

        -- Holes: Finally, any dependencies selected this way have to be
        -- included in the allPkgs index, as well as the buildComponents.
        -- But don't report these as potential inconsistencies!

        when (not (null failed_hmap)) $
          die $ "Could not resolve these package IDs (from signature implementations): "
            ++ intercalate ", " (map display failed_hmap)

        return (holeDeps, hole_insts)

-- -----------------------------------------------------------------------------
-- Configuring program dependencies

configureRequiredPrograms :: Verbosity -> [Dependency] -> ProgramConfiguration
                             -> IO ProgramConfiguration
configureRequiredPrograms verbosity deps conf =
  foldM (configureRequiredProgram verbosity) conf deps

configureRequiredProgram :: Verbosity -> ProgramConfiguration -> Dependency
                            -> IO ProgramConfiguration
configureRequiredProgram verbosity conf
  (Dependency (PackageName progName) verRange) =
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
    lib' <- mapM addPkgConfigBILib (library pkg_descr)
    exes' <- mapM addPkgConfigBIExe (executables pkg_descr)
    tests' <- mapM addPkgConfigBITest (testSuites pkg_descr)
    benches' <- mapM addPkgConfigBIBench (benchmarks pkg_descr)
    let pkg_descr' = pkg_descr { library = lib', executables = exes',
                                 testSuites = tests', benchmarks = benches' }
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
        notFound     = "The pkg-config package '" ++ pkg ++ "'"
                    ++ versionRequirement
                    ++ " is required but it could not be found."
        badVersion v = "The pkg-config package '" ++ pkg ++ "'"
                    ++ versionRequirement
                    ++ " is required but the version installed on the"
                    ++ " system is version " ++ display v
        depSatisfied v = "Dependency " ++ display dep
                      ++ ": using version " ++ display v

        versionRequirement
          | isAnyVersion range = ""
          | otherwise          = " version " ++ display range

    -- Adds pkgconfig dependencies to the build info for a component
    addPkgConfigBI compBI setCompBI comp = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (compBI comp))
      return $ setCompBI comp (compBI comp `mappend` bi)

    -- Adds pkgconfig dependencies to the build info for a library
    addPkgConfigBILib = addPkgConfigBI libBuildInfo $
                          \lib bi -> lib { libBuildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for an executable
    addPkgConfigBIExe = addPkgConfigBI buildInfo $
                          \exe bi -> exe { buildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for a test suite
    addPkgConfigBITest = addPkgConfigBI testBuildInfo $
                          \test bi -> test { testBuildInfo = bi }

    -- Adds pkgconfig dependencies to the build info for a benchmark
    addPkgConfigBIBench = addPkgConfigBI benchmarkBuildInfo $
                          \bench bi -> bench { benchmarkBuildInfo = bi }

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

configCompilerAuxEx :: ConfigFlags
                    -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerAuxEx cfg = configCompilerEx (flagToMaybe $ configHcFlavor cfg)
                                           (flagToMaybe $ configHcPath cfg)
                                           (flagToMaybe $ configHcPkg cfg)
                                           programsConfig
                                           (fromFlag (configVerbosity cfg))
  where
    programsConfig = mkProgramsConfig cfg defaultProgramConfiguration

configCompilerEx :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
                 -> ProgramConfiguration -> Verbosity
                 -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerEx Nothing _ _ _ _ = die "Unknown compiler"
configCompilerEx (Just hcFlavor) hcPath hcPkg conf verbosity = do
  (comp, maybePlatform, programsConfig) <- case hcFlavor of
    GHC   -> GHC.configure  verbosity hcPath hcPkg conf
    GHCJS -> GHCJS.configure verbosity hcPath hcPkg conf
    JHC   -> JHC.configure  verbosity hcPath hcPkg conf
    LHC   -> do (_, _, ghcConf) <- GHC.configure  verbosity Nothing hcPkg conf
                LHC.configure  verbosity hcPath Nothing ghcConf
    UHC   -> UHC.configure  verbosity hcPath hcPkg conf
    HaskellSuite {} -> HaskellSuite.configure verbosity hcPath hcPkg conf
    _    -> die "Unknown compiler"
  return (comp, fromMaybe buildPlatform maybePlatform, programsConfig)

-- Ideally we would like to not have separate configCompiler* and
-- configCompiler*Ex sets of functions, but there are many custom setup scripts
-- in the wild that are using them, so the versions with old types are kept for
-- backwards compatibility. Platform was added to the return triple in 1.18.

{-# DEPRECATED configCompiler
    "'configCompiler' is deprecated. Use 'configCompilerEx' instead." #-}
configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> ProgramConfiguration -> Verbosity
               -> IO (Compiler, ProgramConfiguration)
configCompiler mFlavor hcPath hcPkg conf verbosity =
  fmap (\(a,_,b) -> (a,b)) $ configCompilerEx mFlavor hcPath hcPkg conf verbosity

{-# DEPRECATED configCompilerAux
    "configCompilerAux is deprecated. Use 'configCompilerAuxEx' instead." #-}
configCompilerAux :: ConfigFlags
                  -> IO (Compiler, ProgramConfiguration)
configCompilerAux = fmap (\(a,_,b) -> (a,b)) . configCompilerAuxEx

-- -----------------------------------------------------------------------------
-- Making the internal component graph


mkComponentsGraph :: PackageDescription
                  -> [PackageId]
                  -> Either [ComponentName]
                            [(Component, [ComponentName])]
mkComponentsGraph pkg_descr internalPkgDeps =
    let graph = [ (c, componentName c, componentDeps c)
                | c <- pkgEnabledComponents pkg_descr ]
     in case checkComponentsCyclic graph of
          Just ccycle -> Left  [ cname | (_,cname,_) <- ccycle ]
          Nothing     -> Right [ (c, cdeps) | (c, _, cdeps) <- graph ]
  where
    -- The dependencies for the given component
    componentDeps component =
         [ CExeName toolname | Dependency (PackageName toolname) _
                               <- buildTools bi
                             , toolname `elem` map exeName
                               (executables pkg_descr) ]

      ++ [ CLibName          | Dependency pkgname _ <- targetBuildDepends bi
                             , pkgname `elem` map packageName internalPkgDeps ]
      where
        bi = componentBuildInfo component

reportComponentCycle :: [ComponentName] -> IO a
reportComponentCycle cnames =
    die $ "Components in the package depend on each other in a cyclic way:\n  "
       ++ intercalate " depends on "
            [ "'" ++ showComponentName cname ++ "'"
            | cname <- cnames ++ [head cnames] ]

mkComponentsLocalBuildInfo :: Compiler
                           -> InstalledPackageIndex
                           -> PackageDescription
                           -> [PackageId] -- internal package deps
                           -> [InstalledPackageInfo] -- external package deps
                           -> [InstalledPackageInfo] -- hole package deps
                           -> Map ModuleName (InstalledPackageInfo, ModuleName)
                           -> [(Component, [ComponentName])]
                           -> FlagAssignment
                           -> IO [(ComponentName, ComponentLocalBuildInfo,
                                                  [ComponentName])]
mkComponentsLocalBuildInfo comp installedPackages pkg_descr
                           internalPkgDeps externalPkgDeps holePkgDeps hole_insts
                           graph flagAssignment =
    sequence
      [ do clbi <- componentLocalBuildInfo c
           return (componentName c, clbi, cdeps)
      | (c, cdeps) <- graph ]
  where
    -- The allPkgDeps contains all the package deps for the whole package
    -- but we need to select the subset for this specific component.
    -- we just take the subset for the package names this component
    -- needs. Note, this only works because we cannot yet depend on two
    -- versions of the same package.
    componentLocalBuildInfo component =
      case component of
      CLib lib -> do
        let exports = map (\n -> Installed.ExposedModule n Nothing Nothing)
                          (PD.exposedModules lib)
            esigs = map (\n -> Installed.ExposedModule n Nothing
                                (fmap (\(pkg,m) -> Installed.OriginalModule
                                                      (Installed.installedPackageId pkg) m)
                                      (Map.lookup n hole_insts)))
                        (PD.exposedSignatures lib)
        let mb_reexports = resolveModuleReexports installedPackages
                                                  (packageId pkg_descr)
                                                  externalPkgDeps lib
        reexports <- case mb_reexports of
            Left problems -> reportModuleReexportProblems problems
            Right r -> return r

        -- Calculate the version hash and package key.
        let externalPkgs = selectSubset bi externalPkgDeps
            pkg_key = mkPackageKey (packageKeySupported comp)
                        (package pkg_descr)
                        (map Installed.libraryName externalPkgs)
            version_hash = packageKeyLibraryName (package pkg_descr) pkg_key

        -- Calculate IPID
        ipid <- do
          -- sdist produces too much noise, so silent
          (ordfiles, exefiles) <-
            listPackageSources silent pkg_descr knownSuffixHandlers
          let files = sort $ ordfiles ++ exefiles
          fileHashes <-
#if __GLASGOW_HASKELL__ >= 710
            mapM getFileHash files
#else
            mapM (\x -> readFile x >>= (return . fingerprintString)) files
#endif
          -- show is found to be faster than intercalate and then replacement of
          -- special character used in intercalating. We cannot simply hash by
          -- doubly concating list, as it just flatten out the nested list, so
          -- different sources can produce same hash
          return $ InstalledPackageId $ (display (package pkg_descr)) ++ "-" ++
            (hashToBase62 $
              (show $ map Installed.installedPackageId externalPkgs)
                        ++ show (zip files $
                             map (flip showHex "" . fpToInteger) fileHashes)
                        ++ show flagAssignment)

        return LibComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentPackageKey = pkg_key,
          componentIPID = ipid,
          componentLibraryName = version_hash,
          componentPackageRenaming = cprns,
          componentExposedModules = exports ++ reexports ++ esigs
        }
      CExe _ ->
        return ExeComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentPackageRenaming = cprns
        }
      CTest _ ->
        return TestComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentPackageRenaming = cprns
        }
      CBench _ ->
        return BenchComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentPackageRenaming = cprns
        }
      where
        bi = componentBuildInfo component
        dedup = Map.toList . Map.fromList
        cpds = if newPackageDepsBehaviour pkg_descr
               then dedup $
                    [ (Installed.installedPackageId pkg, packageId pkg)
                    | pkg <- selectSubset bi externalPkgDeps ]
                 ++ [ (inplacePackageId pkgid, pkgid)
                    | pkgid <- selectSubset bi internalPkgDeps ]
               else [ (Installed.installedPackageId pkg, packageId pkg)
                    | pkg <- externalPkgDeps ]
        cprns = if newPackageDepsBehaviour pkg_descr
                then Map.unionWith mappend
                        -- We need hole dependencies passed to GHC, so add them here
                        -- (but note that they're fully thinned out.  If they
                        -- appeared legitimately the monoid instance will
                        -- fill them out.
                        (Map.fromList [(packageName pkg, mempty) | pkg <- holePkgDeps])
                        (targetBuildRenaming bi)
                -- Hack: if we have old package-deps behavior, it's impossible
                -- for non-default renamings to be used, because the Cabal
                -- version is too early.  This is a good, because while all the
                -- deps were bundled up in buildDepends, we didn't do this for
                -- renamings, so it's not even clear how to get the merged
                -- version.  So just assume that all of them are the default..
                else Map.fromList (map (\(_,pid) -> (packageName pid, defaultRenaming)) cpds)

    selectSubset :: Package pkg => BuildInfo -> [pkg] -> [pkg]
    selectSubset bi pkgs =
        [ pkg | pkg <- pkgs, packageName pkg `elem` names bi ]

    names bi = [ name | Dependency name _ <- targetBuildDepends bi ]

    representBase62 x
        | x < 10 = chr (48 + x)
        | x < 36 = chr (65 + x - 10)
        | x < 62 = chr (97 + x - 36)
        | otherwise = '@'
    fpToInteger (Fingerprint a b) =
      toInteger a * (shift (1 :: Integer) 64) + toInteger b
    hashToBase62 s = showIntAtBase 62 representBase62
                      (fpToInteger $ fingerprintString s) ""

-- | Given the author-specified re-export declarations from the .cabal file,
-- resolve them to the form that we need for the package database.
--
-- An invariant of the package database is that we always link the re-export
-- directly to its original defining location (rather than indirectly via a
-- chain of re-exporting packages).
--
resolveModuleReexports :: InstalledPackageIndex
                       -> PackageId
                       -> [InstalledPackageInfo]
                       -> Library
                       -> Either [(ModuleReexport, String)] -- errors
                                 [Installed.ExposedModule] -- ok
resolveModuleReexports installedPackages srcpkgid externalPkgDeps lib =
    case partitionEithers (map resolveModuleReexport (PD.reexportedModules lib)) of
      ([],  ok) -> Right ok
      (errs, _) -> Left  errs
  where
    -- A mapping from visible module names to their original defining
    -- module name.  We also record the package name of the package which
    -- *immediately* provided the module (not the original) to handle if the
    -- user explicitly says which build-depends they want to reexport from.
    visibleModules :: Map ModuleName [(PackageName, Installed.ExposedModule)]
    visibleModules =
      Map.fromListWith (++) $
        [ (Installed.exposedName exposedModule, [(exportingPackageName,
                                                  exposedModule)])
          -- The package index here contains all the indirect deps of the
          -- package we're configuring, but we want just the direct deps
        | let directDeps = Set.fromList (map Installed.installedPackageId externalPkgDeps)
        , pkg <- PackageIndex.allPackages installedPackages
        , Installed.installedPackageId pkg `Set.member` directDeps
        , let exportingPackageName = packageName pkg
        , exposedModule <- visibleModuleDetails pkg
        ]
     ++ [ (visibleModuleName, [(exportingPackageName, exposedModule)])
        | visibleModuleName <- PD.exposedModules lib
                            ++ otherModules (libBuildInfo lib)
        , let exportingPackageName = packageName srcpkgid
              definingModuleName   = visibleModuleName
              -- we don't know the InstalledPackageId of this package yet
              -- we will fill it in later, before registration.
              definingPackageId    = InstalledPackageId ""
              originalModule = Installed.OriginalModule definingPackageId
                                                        definingModuleName
              exposedModule  = Installed.ExposedModule visibleModuleName
                                                       (Just originalModule)
                                                             Nothing
        ]

    -- All the modules exported from this package and their defining name and
    -- package (either defined here in this package or re-exported from some
    -- other package).  Return an ExposedModule because we want to hold onto
    -- signature information.
    visibleModuleDetails :: InstalledPackageInfo -> [Installed.ExposedModule]
    visibleModuleDetails pkg = do
        exposedModule <- Installed.exposedModules pkg
        case Installed.exposedReexport exposedModule of
        -- The first case is the modules actually defined in this package.
        -- In this case the reexport will point to this package.
            Nothing -> return exposedModule { Installed.exposedReexport =
                            Just (Installed.OriginalModule (Installed.installedPackageId pkg)
                                                 (Installed.exposedName exposedModule)) }
        -- On the other hand, a visible module might actually be itself
        -- a re-export! In this case, the re-export info for the package
        -- doing the re-export will point us to the original defining
        -- module name and package, so we can reuse the entry.
            Just _ -> return exposedModule

    resolveModuleReexport reexport@ModuleReexport {
         moduleReexportOriginalPackage = moriginalPackageName,
         moduleReexportOriginalName    = originalName,
         moduleReexportName            = newName
      } =

      let filterForSpecificPackage =
            case moriginalPackageName of
              Nothing                  -> id
              Just originalPackageName ->
                filter (\(pkgname, _) -> pkgname == originalPackageName)

          matches = filterForSpecificPackage
                      (Map.findWithDefault [] originalName visibleModules)
      in
      case (matches, moriginalPackageName) of
        ((_, exposedModule):rest, _)
          -- TODO: Refine this check for signatures
          | all (\(_, exposedModule') -> Installed.exposedReexport exposedModule
                                      == Installed.exposedReexport exposedModule') rest
           -> Right exposedModule { Installed.exposedName = newName }

        ([], Just originalPackageName)
           -> Left $ (,) reexport
                   $ "The package " ++ display originalPackageName
                  ++ " does not export a module " ++ display originalName

        ([], Nothing)
           -> Left $ (,) reexport
                   $ "The module " ++ display originalName
                  ++ " is not exported by any suitable package (this package "
                  ++ "itself nor any of its 'build-depends' dependencies)."

        (ms, _)
           -> Left $ (,) reexport
                   $ "The module " ++ display originalName ++ " is exported "
                  ++ "by more than one package ("
                  ++ intercalate ", " [ display pkgname | (pkgname,_) <- ms ]
                  ++ ") and so the re-export is ambiguous. The ambiguity can "
                  ++ "be resolved by qualifying by the package name. The "
                  ++ "syntax is 'packagename:moduleName [as newname]'."

        -- Note: if in future Cabal allows directly depending on multiple
        -- instances of the same package (e.g. backpack) then an additional
        -- ambiguity case is possible here: (_, Just originalPackageName)
        -- with the module being ambiguous despite being qualified by a
        -- package name. Presumably by that time we'll have a mechanism to
        -- qualify the instance we're referring to.

reportModuleReexportProblems :: [(ModuleReexport, String)] -> IO a
reportModuleReexportProblems reexportProblems =
  die $ unlines
    [ "Problem with the module re-export '" ++ display reexport ++ "': " ++ msg
    | (reexport, msg) <- reexportProblems ]

-- -----------------------------------------------------------------------------
-- Testing C lib and header dependencies

-- Try to build a test C program which includes every header and links every
-- lib. If that fails, try to narrow it down by preprocessing (only) and linking
-- with individual headers and libs.  If none is the obvious culprit then give a
-- generic error message.
-- TODO: produce a log file from the compiler errors, if any.
checkForeignDeps :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
checkForeignDeps pkg lbi verbosity = do
  ifBuildsWith allHeaders (commonCcArgs ++ makeLdArgs allLibs) -- I'm feeling
                                                               -- lucky
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

        commonCppArgs = platformDefines lbi
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
        explainErrors _ _
           | isNothing . lookupProgram gccProgram . withPrograms $ lbi

                              = die $ unlines $
              [ "No working gcc",
                  "This package depends on foreign library but we cannot "
               ++ "find a working C compiler. If you have it in a "
               ++ "non-standard location you can use the --with-gcc "
               ++ "flag to specify it." ]

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
    else die (intercalate "\n\n" errors)

-- | Preform checks if a relocatable build is allowed
checkRelocatable :: Verbosity
                 -> PackageDescription
                 -> LocalBuildInfo
                 -> IO ()
checkRelocatable verbosity pkg lbi
    = sequence_ [ checkOS
                , checkCompiler
                , packagePrefixRelative
                , depsPrefixRelative
                ]
  where
    -- Check if the OS support relocatable builds.
    --
    -- If you add new OS' to this list, and your OS supports dynamic libraries
    -- and RPATH, make sure you add your OS to RPATH-support list of:
    -- Distribution.Simple.GHC.getRPaths
    checkOS
        = unless (os `elem` [ OSX, Linux ])
        $ die $ "Operating system: " ++ display os ++
                ", does not support relocatable builds"
      where
        (Platform _ os) = hostPlatform lbi

    -- Check if the Compiler support relocatable builds
    checkCompiler
        = unless (compilerFlavor comp `elem` [ GHC ])
        $ die $ "Compiler: " ++ show comp ++
                ", does not support relocatable builds"
      where
        comp = compiler lbi

    -- Check if all the install dirs are relative to same prefix
    packagePrefixRelative
        = unless (relativeInstallDirs installDirs)
        $ die $ "Installation directories are not prefix_relative:\n" ++
                show installDirs
      where
        installDirs = absoluteInstallDirs pkg lbi NoCopyDest
        p           = prefix installDirs
        relativeInstallDirs (InstallDirs {..}) =
          all isJust
              (fmap (stripPrefix p)
                    [ bindir, libdir, dynlibdir, libexecdir, includedir, datadir
                    , docdir, mandir, htmldir, haddockdir, sysconfdir] )

    -- Check if the library dirs of the dependencies that are in the package
    -- database to which the package is installed are relative to the
    -- prefix of the package
    depsPrefixRelative = do
        pkgr <- GHC.pkgRoot verbosity lbi (last (withPackageDB lbi))
        mapM_ (doCheck pkgr) ipkgs
      where
        doCheck pkgr ipkg
          | maybe False (== pkgr) (Installed.pkgRoot ipkg)
          = mapM_ (\l -> when (isNothing $ stripPrefix p l) (die (msg l)))
                  (Installed.libraryDirs ipkg)
          | otherwise
          = return ()
        installDirs   = absoluteInstallDirs pkg lbi NoCopyDest
        p             = prefix installDirs
        ipkgs         = PackageIndex.allPackages (installedPkgs lbi)
        msg l         = "Library directory of a dependency: " ++ show l ++
                        "\nis not relative to the installation prefix:\n" ++
                        show p
