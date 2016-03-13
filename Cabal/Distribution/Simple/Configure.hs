{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
                                      computeComponentId,
                                      computeCompatPackageKey,
                                      localBuildInfoFile,
                                      getInstalledPackages,
                                      getInstalledPackagesMonitorFiles,
                                      getPackageDBContents,
                                      configCompiler, configCompilerAux,
                                      configCompilerEx, configCompilerAuxEx,
                                      ccLdOptionsBuildInfo,
                                      checkForeignDeps,
                                      interpretPackageDbFlags,
                                      ConfigStateFileError(..),
                                      tryGetConfigStateFile,
                                      platformDefines,
                                      relaxPackageDeps,
                                     )
    where

import Distribution.Compiler
import Distribution.Utils.NubList
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.PreProcess
import Distribution.Package
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.InstalledPackageInfo (InstalledPackageInfo
                                         ,emptyInstalledPackageInfo)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.ModuleName
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.Simple.Program
import Distribution.Simple.Setup as Setup
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Version
import Distribution.Verbosity

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite

-- Prefer the more generic Data.Traversable.mapM to Prelude.mapM
import Prelude hiding ( mapM )
import Control.Exception
    ( Exception, evaluate, throw, throwIO, try )
import Control.Exception ( ErrorCall )
import Control.Monad
    ( liftM, when, unless, foldM, filterM, mplus )
import Distribution.Compat.Binary ( decodeOrFailIO, encode )
import GHC.Fingerprint ( Fingerprint(..), fingerprintString )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.List
    ( (\\), nub, partition, isPrefixOf, inits, stripPrefix )
import Data.Maybe
    ( isNothing, catMaybes, fromMaybe, mapMaybe, isJust )
import Data.Either
    ( partitionEithers )
import qualified Data.Set as Set
import Data.Monoid as Mon ( Monoid(..) )
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable
    ( mapM )
import Data.Typeable
import Data.Char ( chr, isAlphaNum )
import Numeric ( showIntAtBase )
import System.Directory
    ( doesFileExist, createDirectoryIfMissing, getTemporaryDirectory )
import System.FilePath
    ( (</>), isAbsolute )
import qualified System.Info
    ( compilerName, compilerVersion )
import System.IO
    ( hPutStrLn, hClose )
import Distribution.Text
    ( Text(disp), defaultStyle, display, simpleParse )
import Text.PrettyPrint
    ( Doc, (<>), (<+>), ($+$), char, comma, empty, hsep, nest
    , punctuate, quotes, render, renderStyle, sep, text )
import Distribution.Compat.Environment ( lookupEnv )
import Distribution.Compat.Exception ( catchExit, catchIO )

-- | The errors that can be thrown when reading the @setup-config@ file.
data ConfigStateFileError
    = ConfigStateFileNoHeader -- ^ No header found.
    | ConfigStateFileBadHeader -- ^ Incorrect header.
    | ConfigStateFileNoParse -- ^ Cannot parse file contents.
    | ConfigStateFileMissing -- ^ No file!
    | ConfigStateFileBadVersion PackageIdentifier PackageIdentifier
      (Either ConfigStateFileError LocalBuildInfo) -- ^ Mismatched version.
  deriving (Typeable)

-- | Format a 'ConfigStateFileError' as a user-facing error message.
dispConfigStateFileError :: ConfigStateFileError -> Doc
dispConfigStateFileError ConfigStateFileNoHeader =
    text "Saved package config file header is missing."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileBadHeader =
    text "Saved package config file header is corrupt."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileNoParse =
    text "Saved package config file is corrupt."
    <+> text "Re-run the 'configure' command."
dispConfigStateFileError ConfigStateFileMissing =
    text "Run the 'configure' command first."
dispConfigStateFileError (ConfigStateFileBadVersion oldCabal oldCompiler _) =
    text "Saved package config file is outdated:"
    $+$ badCabal $+$ badCompiler
    $+$ text "Re-run the 'configure' command."
    where
      badCabal =
          text "• the Cabal version changed from"
          <+> disp oldCabal <+> "to" <+> disp currentCabalId
      badCompiler
        | oldCompiler == currentCompilerId = empty
        | otherwise =
            text "• the compiler changed from"
            <+> disp oldCompiler <+> "to" <+> disp currentCompilerId

instance Show ConfigStateFileError where
    show = renderStyle defaultStyle . dispConfigStateFileError

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
              Left (_ :: ErrorCall) -> throw ConfigStateFileBadHeader
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

-- | Read the 'localBuildInfoFile', returning either an error or the local build
-- info.
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
  ["Saved", "package", "config", "for", pkgId, "written", "by", cabalId,
   "using", compId] ->
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

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or the default prefix.
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

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or 'defaultDistPref' is used. Call
-- this function to resolve a @*DistPref@ flag whenever it is not known to be
-- set. (The @*DistPref@ flags are always set to a definite value before
-- invoking 'UserHooks'.)
findDistPrefOrDefault :: Setup.Flag FilePath  -- ^ override \"dist\" prefix
                      -> IO FilePath
findDistPrefOrDefault = findDistPref defaultDistPref

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: (GenericPackageDescription, HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0', pbi) cfg = do
    let pkg_descr0 =
          -- Ignore '--allow-newer' when we're given '--exact-configuration'.
          if fromFlagOrDefault False (configExactConfiguration cfg)
          then pkg_descr0'
          else relaxPackageDeps
               (fromMaybe AllowNewerNone $ configAllowNewer cfg)
               pkg_descr0'

    setupMessage verbosity "Configuring" (packageId pkg_descr0)

    checkDeprecatedFlags verbosity cfg
    checkExactConfiguration pkg_descr0 cfg

    -- Where to build the package
    let buildDir :: FilePath -- e.g. dist/build
        -- fromFlag OK due to Distribution.Simple calling
        -- findDistPrefOrDefault to fill it in
        buildDir = fromFlag (configDistPref cfg) </> "build"
    createDirectoryIfMissingVerbose (lessVerbose verbosity) True buildDir

    -- What package database(s) to use
    let packageDbs
         = interpretPackageDbFlags
            (fromFlag (configUserInstall cfg))
            (configPackageDBs cfg)

    -- comp:            the compiler we're building with
    -- compPlatform:    the platform we're building for
    -- programsConfig:  location and args of all programs we're
    --                  building with
    (comp, compPlatform, programsConfig)
        <- configCompilerEx
            (flagToMaybe (configHcFlavor cfg))
            (flagToMaybe (configHcPath cfg))
            (flagToMaybe (configHcPkg cfg))
            (mkProgramsConfig cfg (configPrograms cfg))
            (lessVerbose verbosity)

    -- The InstalledPackageIndex of all installed packages
    installedPackageSet <- getInstalledPackages (lessVerbose verbosity) comp
                                  packageDbs programsConfig

    -- The InstalledPackageIndex of all (possible) internal packages
    let internalPackageSet = getInternalPackages pkg_descr0

    -- allConstraints:  The set of all 'Dependency's we have.  Used ONLY
    --                  to 'configureFinalizedPackage'.
    -- requiredDepsMap: A map from 'PackageName' to the specifically
    --                  required 'InstalledPackageInfo', due to --dependency
    --
    -- NB: These constraints are to be applied to ALL components of
    -- a package.  Thus, it's not an error if allConstraints contains
    -- more constraints than is necessary for a component (another
    -- component might need it.)
    --
    -- NB: The fact that we bundle all the constraints together means
    -- that is not possible to configure a test-suite to use one
    -- version of a dependency, and the executable to use another.
    (allConstraints, requiredDepsMap) <- either die return $
      combinedConstraints (configConstraints cfg)
                          (configDependencies cfg)
                          installedPackageSet

    -- pkg_descr:   The resolved package description, that does not contain any
    --              conditionals, because we have have an assignment for
    --              every flag, either picking them ourselves using a
    --              simple naive algorithm, or having them be passed to
    --              us by 'configConfigurationsFlags')
    -- flags:       The 'FlagAssignment' that the conditionals were
    --              resolved with.
    --
    -- NB: Why doesn't finalizing a package also tell us what the
    -- dependencies are (e.g. when we run the naive algorithm,
    -- we are checking if dependencies are satisfiable)?  The
    -- primary reason is that we may NOT have done any solving:
    -- if the flags are all chosen for us, this step is a simple
    -- matter of flattening according to that assignment.  It's
    -- cleaner to then configure the dependencies afterwards.
    (pkg_descr, flags)
        <- configureFinalizedPackage verbosity cfg
                allConstraints
                (dependencySatisfiable
                    (fromFlagOrDefault False (configExactConfiguration cfg))
                    installedPackageSet
                    internalPackageSet
                    requiredDepsMap)
                comp
                compPlatform
                pkg_descr0

    checkCompilerProblems comp pkg_descr
    checkPackageProblems verbosity pkg_descr0
        (updatePackageDescription pbi pkg_descr)

    -- The list of 'InstalledPackageInfo' recording the selected
    -- dependencies...
    -- internalPkgDeps: ...on internal packages (these are fake!)
    -- externalPkgDeps: ...on external packages
    --
    -- Invariant: For any package name, there is at most one package
    -- in externalPackageDeps which has that name.
    --
    -- NB: The dependency selection is global over ALL components
    -- in the package (similar to how allConstraints and
    -- requiredDepsMap are global over all components).  In particular,
    -- if *any* component (post-flag resolution) has an unsatisfiable
    -- dependency, we will fail.  This can sometimes be undesirable
    -- for users, see #1786 (benchmark conflicts with executable),
    (internalPkgDeps, externalPkgDeps)
        <- configureDependencies
                verbosity
                internalPackageSet
                installedPackageSet
                requiredDepsMap
                pkg_descr

    let installDeps = Map.elems -- deduplicate
                    . Map.fromList
                    . map (\v -> (Installed.installedUnitId v, v))
                    $ externalPkgDeps

    packageDependsIndex <-
      case PackageIndex.dependencyClosure installedPackageSet
              (map Installed.installedUnitId installDeps) of
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
            Installed.installedUnitId =
               mkLegacyUnitId (packageId pkg_descr),
            Installed.sourcePackageId = packageId pkg_descr,
            Installed.depends =
              map Installed.installedUnitId installDeps
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
    defaultDirs <- defaultInstallDirs (compilerFlavor comp)
                   (fromFlag (configUserInstall cfg)) (hasLibs pkg_descr)
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

    programsConfig' <-
          configureAllKnownPrograms (lessVerbose verbosity) programsConfig
      >>= configureRequiredPrograms verbosity requiredBuildTools

    (pkg_descr', programsConfig'') <-
      configurePkgconfigPackages verbosity pkg_descr programsConfig'

    -- internal component graph
    buildComponents <-
      case mkComponentsGraph pkg_descr internalPkgDeps of
        Left  componentCycle -> reportComponentCycle componentCycle
        Right comps          ->
          mkComponentsLocalBuildInfo cfg comp packageDependsIndex pkg_descr
                                     internalPkgDeps externalPkgDeps
                                     comps (configConfigurationsFlags cfg)

    split_objs <-
       if not (fromFlag $ configSplitObjs cfg)
            then return False
            else case compilerFlavor comp of
                        GHC | compilerVersion comp >= Version [6,5] []
                          -> return True
                        GHCJS
                          -> return True
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
                flagAssignment      = flags,
                extraConfigArgs     = [],  -- Currently configure does not
                                           -- take extra args, but if it
                                           -- did they would go here.
                installDirTemplates = installDirs,
                compiler            = comp,
                hostPlatform        = compPlatform,
                buildDir            = buildDir,
                componentsConfigs   = buildComponents,
                installedPkgs       = packageDependsIndex,
                pkgDescrFile        = Nothing,
                localPkgDescr       = pkg_descr',
                withPrograms        = programsConfig'',
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
              | (prog, configuredProg) <- knownPrograms programsConfig'' ]

    return lbi

    where
      verbosity = fromFlag (configVerbosity cfg)

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
                  ++ map ProgramSearchPathDir
                     (fromNubList $ configProgramPathExtra cfg)

-- -----------------------------------------------------------------------------
-- Helper functions for configure

-- | Check if the user used any deprecated flags.
checkDeprecatedFlags :: Verbosity -> ConfigFlags -> IO ()
checkDeprecatedFlags verbosity cfg = do
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

-- | Sanity check: if '--exact-configuration' was given, ensure that the
-- complete flag assignment was specified on the command line.
checkExactConfiguration :: GenericPackageDescription -> ConfigFlags -> IO ()
checkExactConfiguration pkg_descr0 cfg = do
    when (fromFlagOrDefault False (configExactConfiguration cfg)) $ do
      let cmdlineFlags = map fst (configConfigurationsFlags cfg)
          allFlags     = map flagName . genPackageFlags $ pkg_descr0
          diffFlags    = allFlags \\ cmdlineFlags
      when (not . null $ diffFlags) $
        die $ "'--exact-configuration' was given, "
        ++ "but the following flags were not specified: "
        ++ intercalate ", " (map show diffFlags)

-- | Create a PackageIndex that makes *any libraries that might be*
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
getInternalPackages :: GenericPackageDescription
                    -> InstalledPackageIndex
getInternalPackages pkg_descr0 =
    let pid :: PackageIdentifier -- e.g. foo-0.1
        pid = packageId pkg_descr0
        internalPackage = emptyInstalledPackageInfo {
            --TODO: should use a per-compiler method to map the source
            --      package ID into an installed package id we can use
            --      for the internal package set. The use of
            --      mkLegacyUnitId here is a hack.
            Installed.installedUnitId = mkLegacyUnitId pid,
            Installed.sourcePackageId = pid
          }
    in PackageIndex.fromList [internalPackage]


-- | Returns true if a dependency is satisfiable.  This is to be passed
-- to finalizePackageDescription.
dependencySatisfiable
    :: Bool
    -> InstalledPackageIndex -- ^ installed set
    -> InstalledPackageIndex -- ^ internal set
    -> Map PackageName InstalledPackageInfo -- ^ required dependencies
    -> (Dependency -> Bool)
dependencySatisfiable
    exact_config installedPackageSet internalPackageSet requiredDepsMap
    d@(Dependency depName _)
      | exact_config =
        -- When we're given '--exact-configuration', we assume that all
        -- dependencies and flags are exactly specified on the command
        -- line. Thus we only consult the 'requiredDepsMap'. Note that
        -- we're not doing the version range check, so if there's some
        -- dependency that wasn't specified on the command line,
        -- 'finalizePackageDescription' will fail.
        --
        -- TODO: mention '--exact-configuration' in the error message
        -- when this fails?
        --
        -- (However, note that internal deps don't have to be
        -- specified!)
        (depName `Map.member` requiredDepsMap) || isInternalDep

      | otherwise =
        -- Normal operation: just look up dependency in the combined
        -- package index.
        not . null . PackageIndex.lookupDependency pkgs $ d
      where
        pkgs = PackageIndex.merge internalPackageSet installedPackageSet
        isInternalDep = not . null
                      $ PackageIndex.lookupDependency internalPackageSet d

-- | Relax the dependencies of this package if needed.
relaxPackageDeps :: AllowNewer -> GenericPackageDescription
                 -> GenericPackageDescription
relaxPackageDeps AllowNewerNone gpd = gpd
relaxPackageDeps AllowNewerAll  gpd = transformAllBuildDepends relaxAll gpd
  where
    relaxAll = \(Dependency pkgName verRange) ->
      Dependency pkgName (removeUpperBound verRange)
relaxPackageDeps (AllowNewerSome allowNewerDeps') gpd =
  transformAllBuildDepends relaxSome gpd
  where
    thisPkgName    = packageName gpd
    allowNewerDeps = mapMaybe f allowNewerDeps'

    f (Setup.AllowNewerDep p) = Just p
    f (Setup.AllowNewerDepScoped scope p) | scope == thisPkgName = Just p
                                          | otherwise            = Nothing

    relaxSome = \d@(Dependency depName verRange) ->
      if depName `elem` allowNewerDeps
      then Dependency depName (removeUpperBound verRange)
      else d

-- | Finalize a generic package description.  The workhorse is
-- 'finalizePackageDescription' but there's a bit of other nattering
-- about necessary.
--
-- TODO: what exactly is the business with @flaggedTests@ and
-- @flaggedBenchmarks@?
configureFinalizedPackage
    :: Verbosity
    -> ConfigFlags
    -> [Dependency]
    -> (Dependency -> Bool) -- ^ tests if a dependency is satisfiable.
                            -- Might say it's satisfiable even when not.
    -> Compiler
    -> Platform
    -> GenericPackageDescription
    -> IO (PackageDescription, FlagAssignment)
configureFinalizedPackage verbosity cfg
  allConstraints satisfies comp compPlatform pkg_descr0 = do
    let enableTest t = t { testEnabled = fromFlag (configTests cfg) }
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
                   satisfies
                   compPlatform
                   (compilerInfo comp)
                   allConstraints
                   pkg_descr0''
            of Right r -> return r
               Left missing ->
                   die $ "Encountered missing dependencies:\n"
                     ++ (render . nest 4 . sep . punctuate comma
                                . map (disp . simplifyDependency)
                                $ missing)

    -- add extra include/lib dirs as specified in cfg
    -- we do it here so that those get checked too
    let pkg_descr = addExtraIncludeLibDirs pkg_descr0'

    when (not (null flags)) $
      info verbosity $ "Flags chosen: "
                    ++ intercalate ", " [ name ++ "=" ++ display value
                                        | (FlagName name, value) <- flags ]

    return (pkg_descr, flags)
  where
    addExtraIncludeLibDirs pkg_descr =
        let extraBi = mempty { extraLibDirs = configExtraLibDirs cfg
                             , extraFrameworkDirs = configExtraFrameworkDirs cfg
                             , PD.includeDirs = configExtraIncludeDirs cfg}
            modifyLib l        = l{ libBuildInfo = libBuildInfo l
                                                   `mappend` extraBi }
            modifyExecutable e = e{ buildInfo    = buildInfo e
                                                   `mappend` extraBi}
        in pkg_descr{ library     = modifyLib        `fmap` library pkg_descr
                    , executables = modifyExecutable  `map`
                                      executables pkg_descr}

-- | Check for use of Cabal features which require compiler support
checkCompilerProblems :: Compiler -> PackageDescription -> IO ()
checkCompilerProblems comp pkg_descr = do
    unless (renamingPackageFlagsSupported comp ||
                and [ True
                    | bi <- allBuildInfo pkg_descr
                    , _ <- Map.elems (targetBuildRenaming bi)]) $
        die $ "Your compiler does not support thinning and renaming on "
           ++ "package flags.  To use this feature you probably must use "
           ++ "GHC 7.9 or later."

    when (maybe False (not.null.PD.reexportedModules) (PD.library pkg_descr)
          && not (reexportedModulesSupported comp)) $ do
        die $ "Your compiler does not support module re-exports. To use "
           ++ "this feature you probably must use GHC 7.9 or later."

-- | Select dependencies for the package.
configureDependencies
    :: Verbosity
    -> InstalledPackageIndex -- ^ internal packages
    -> InstalledPackageIndex -- ^ installed packages
    -> Map PackageName InstalledPackageInfo -- ^ required deps
    -> PackageDescription
    -> IO ([PackageId], [InstalledPackageInfo])
configureDependencies verbosity
  internalPackageSet installedPackageSet requiredDepsMap pkg_descr = do
    let selectDependencies :: [Dependency] ->
                              ([FailedDependency], [ResolvedDependency])
        selectDependencies =
            partitionEithers
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

    return (internalPkgDeps, externalPkgDeps)

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
                    -- ^ Packages for which we have been given specific deps to
                    -- use
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
--
-- NB: Why isn't this always a fall through to 'getInstalledPackages'?
-- That is because 'getInstalledPackages' performs some sanity checks
-- on the package database stack in question.  However, when sandboxes
-- are involved these sanity checks are not desirable.
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


-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the installed packages.
--
getInstalledPackagesMonitorFiles :: Verbosity -> Compiler
                                 -> PackageDBStack
                                 -> ProgramConfiguration -> Platform
                                 -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity comp packageDBs progconf platform =
  case compilerFlavor comp of
    GHC   -> GHC.getInstalledPackagesMonitorFiles
               verbosity platform progconf packageDBs
    other -> do
      warn verbosity $ "don't know how to find change monitoring files for "
                    ++ "the installed package databases for " ++ display other
      return []

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
                       [(PackageName, UnitId)] ->
                       InstalledPackageIndex ->
                       Either String ([Dependency],
                                      Map PackageName InstalledPackageInfo)
combinedConstraints constraints dependencies installedPackages = do

    when (not (null badUnitIds)) $
      Left $ render $ text "The following package dependencies were requested"
         $+$ nest 4 (dispDependencies badUnitIds)
         $+$ text "however the given installed package instance does not exist."

    when (not (null badNames)) $
      Left $ render $ text "The following package dependencies were requested"
         $+$ nest 4 (dispDependencies badNames)
         $+$ text ("however the installed package's name does not match "
                   ++ "the name given.")

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
    dependenciesPkgInfo :: [(PackageName, UnitId,
                             Maybe InstalledPackageInfo)]
    dependenciesPkgInfo =
      [ (pkgname, ipkgid, mpkg)
      | (pkgname, ipkgid) <- dependencies
      , let mpkg = PackageIndex.lookupUnitId
                     installedPackages ipkgid
      ]

    -- If we looked up a package specified by an installed package id
    -- (i.e. someone has written a hash) and didn't find it then it's
    -- an error.
    badUnitIds =
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
    pkgconfigBuildInfo []      = return Mon.mempty
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

-- | This method computes a default, "good enough" 'ComponentId'
-- for a package.  The intent is that cabal-install (or the user) will
-- specify a more detailed IPID via the @--ipid@ flag if necessary.
computeComponentId :: PackageIdentifier
            -> ComponentName
            -- TODO: careful here!
            -> [ComponentId] -- IPIDs of the component dependencies
            -> FlagAssignment
            -> ComponentId
computeComponentId pid cname dep_ipids flagAssignment = do
    -- show is found to be faster than intercalate and then replacement of
    -- special character used in intercalating. We cannot simply hash by
    -- doubly concating list, as it just flatten out the nested list, so
    -- different sources can produce same hash
    let hash = hashToBase62 $
                -- For safety, include the package + version here
                -- for GHC 7.10, where just the hash is used as
                -- the package key
                (display pid)
                      ++ (show $ dep_ipids)
                      ++ show flagAssignment
    ComponentId $
                display pid
                    ++ "-" ++ hash
                    ++ (case cname of
                         CLibName -> ""
                         -- TODO: these could result in non-parseable IPIDs
                         -- since the component name format is very flexible
                         CExeName   s -> "-" ++ s ++ ".exe"
                         CTestName  s -> "-" ++ s ++ ".test"
                         CBenchName s -> "-" ++ s ++ ".bench")

hashToBase62 :: String -> String
hashToBase62 s = showFingerprint $ fingerprintString s
  where
    showIntAtBase62 x = showIntAtBase 62 representBase62 x ""
    representBase62 x
        | x < 10 = chr (48 + x)
        | x < 36 = chr (65 + x - 10)
        | x < 62 = chr (97 + x - 36)
        | otherwise = '@'
    showFingerprint (Fingerprint a b) = showIntAtBase62 a ++ showIntAtBase62 b

-- | In GHC 8.0, the string we pass to GHC to use for symbol
-- names for a package can be an arbitrary, IPID-compatible string.
-- However, prior to GHC 8.0 there are some restrictions on what
-- format this string can be (due to how ghc-pkg parsed the key):
--
--      1. In GHC 7.10, the string had either be of the form
--      foo_ABCD, where foo is a non-semantic alphanumeric/hyphenated
--      prefix and ABCD is two base-64 encoded 64-bit integers,
--      or a GHC 7.8 style identifier.
--
--      2. In GHC 7.8, the string had to be a valid package identifier
--      like foo-0.1.
--
-- So, the problem is that Cabal, in general, has a general IPID,
-- but needs to figure out a package key / package ID that the
-- old ghc-pkg will actually accept.  But there's an EVERY WORSE
-- problem: if ghc-pkg decides to parse an identifier foo-0.1-xxx
-- as if it were a package identifier, which means it will SILENTLY
-- DROP the "xxx" (because it's a tag, and Cabal does not allow tags.)
-- So we must CONNIVE to ensure that we don't pick something that
-- looks like this.
--
-- So this function attempts to define a mapping into the old formats.
--
-- The mapping for GHC 7.8 and before:
--
--      * For CLibName, we unconditionally use the 'PackageIdentifier'.
--
--      * For sub-components, we create a new 'PackageIdentifier' which
--        is encoded in the following way.  The test suite "qux" in package
--        "foobar-0.2" gets this package identifier "z-foobar-z-test-qux-0.2".
--        These package IDs have the form:
--
--          cpid ::= "z-" package-id "-z-" component-type "-" component-name
--          component-type ::= "test" | "bench" | "exe" | "lib"
--          package-id and component-name have "-" ( "z" + ) "-"
--          segments encoded by adding an extra "z".
--
-- The mapping for GHC 7.10:
--
--      * For CLibName:
--          If the IPID is of the form foo-0.1-ABCDEF where foo_ABCDEF would
--          validly parse as a package key, we pass "ABCDEF".  (NB: not
--          all hashes parse this way, because GHC 7.10 mandated that
--          these hashes be two base-62 encoded 64 bit integers),
--          but hashes that Cabal generated using 'computeComponentId'
--          are guaranteed to have this form.
--
--          If it is not of this form, we rehash the IPID into the
--          correct form and pass that.
--
--      * For sub-components, we rehash the IPID into the correct format
--        and pass that.
--
computeCompatPackageKey
    :: Compiler
    -> PackageIdentifier
    -> ComponentName
    -> UnitId
    -> (PackageName, String)
computeCompatPackageKey comp pid cname uid@(SimpleUnitId (ComponentId str))
    | not (packageKeySupported comp || unitIdSupported comp) =
        -- NB: the package ID in the database entry has to follow this
        let zdashcode s = go s (Nothing :: Maybe Int) []
                where go [] _ r = reverse r
                      go ('-':z) (Just n) r | n > 0 = go z (Just 0) ('-':'z':r)
                      go ('-':z) _        r = go z (Just 0) ('-':r)
                      go ('z':z) (Just n) r = go z (Just (n+1)) ('z':r)
                      go (c:z)   _        r = go z Nothing (c:r)
            cname_str = case cname of
                            CLibName     -> error "computeCompatPackageKey"
                            CTestName n  -> "-z-test-"  ++ zdashcode n
                            CBenchName n -> "-z-bench-" ++ zdashcode n
                            CExeName n   -> "-z-exe-"   ++ zdashcode n
            package_name
                | cname == CLibName = pkgName pid
                | otherwise = PackageName $ "z-"
                              ++ zdashcode (display (pkgName pid))
                              ++ zdashcode cname_str
            old_style_key
                | cname == CLibName = display pid
                | otherwise         = display package_name ++ "-"
                                   ++ display (pkgVersion pid)
        in (package_name, old_style_key)
    | not (unifiedIPIDRequired comp) =
        let mb_verbatim_key
                = case simpleParse str :: Maybe PackageId of
                    -- Something like 'foo-0.1', use it verbatim.
                    -- (NB: hash tags look like tags, so they are parsed,
                    -- so the extra equality check tests if a tag was dropped.)
                    Just pid0 | display pid0 == str -> Just str
                    _ -> Nothing
            mb_truncated_key
                = let cand = reverse (takeWhile isAlphaNum (reverse str))
                  in if length cand == 22 && all isAlphaNum cand
                        then Just cand
                        else Nothing
            rehashed_key = hashToBase62 str
        in (pkgName pid, fromMaybe rehashed_key
                            (mb_verbatim_key `mplus` mb_truncated_key))
    | otherwise = (pkgName pid, display uid)

mkComponentsLocalBuildInfo :: ConfigFlags
                           -> Compiler
                           -> InstalledPackageIndex
                           -> PackageDescription
                           -> [PackageId] -- internal package deps
                           -> [InstalledPackageInfo] -- external package deps
                           -> [(Component, [ComponentName])]
                           -> FlagAssignment
                           -> IO [(ComponentName, ComponentLocalBuildInfo,
                                                  [ComponentName])]
mkComponentsLocalBuildInfo cfg comp installedPackages pkg_descr
                           internalPkgDeps externalPkgDeps
                           graph flagAssignment = do
    -- Pre-compute library hash so we can setup internal deps
    -- TODO configIPID should have name changed
    let cid = case configIPID cfg of
                Flag cid0 ->
                    -- Hack to reuse install dirs machinery
                    -- NB: no real IPID available at this point
                    let env = packageTemplateEnv (package pkg_descr)
                                                 (mkUnitId "")
                        str = fromPathTemplate
                                (InstallDirs.substPathTemplate env
                                 (toPathTemplate cid0))
                    in ComponentId str
                _ ->
                  computeComponentId (package pkg_descr) CLibName
                  (getDeps CLibName) flagAssignment
        uid = SimpleUnitId cid
        (_, compat_key) = computeCompatPackageKey comp
                          (package pkg_descr) CLibName uid
    sequence
      [ do clbi <- componentLocalBuildInfo uid compat_key c
           return (componentName c, clbi, cdeps)
      | (c, cdeps) <- graph ]
  where
    getDeps cname =
          let externalPkgs = maybe [] (\lib -> selectSubset
                                               (componentBuildInfo lib)
                                               externalPkgDeps)
                                      (lookupComponent pkg_descr cname)
          in map Installed.installedComponentId externalPkgs

    -- The allPkgDeps contains all the package deps for the whole package
    -- but we need to select the subset for this specific component.
    -- we just take the subset for the package names this component
    -- needs. Note, this only works because we cannot yet depend on two
    -- versions of the same package.
    componentLocalBuildInfo uid compat_key component =
      case component of
      CLib lib -> do
        let exports = map (\n -> Installed.ExposedModule n Nothing)
                          (PD.exposedModules lib)
        let mb_reexports = resolveModuleReexports installedPackages
                                                  (packageId pkg_descr)
                                                  uid
                                                  externalPkgDeps lib
        reexports <- case mb_reexports of
            Left problems -> reportModuleReexportProblems problems
            Right r -> return r

        return LibComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentUnitId = uid,
          componentCompatPackageKey = compat_key,
          componentPackageRenaming = cprns,
          componentExposedModules = exports ++ reexports
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
                    [ (Installed.installedUnitId pkg, packageId pkg)
                    | pkg <- selectSubset bi externalPkgDeps ]
                 ++ [ (uid, pkgid)
                    | pkgid <- selectSubset bi internalPkgDeps ]
               else [ (Installed.installedUnitId pkg, packageId pkg)
                    | pkg <- externalPkgDeps ]
        cprns = if newPackageDepsBehaviour pkg_descr
                then targetBuildRenaming bi
                -- Hack: if we have old package-deps behavior, it's impossible
                -- for non-default renamings to be used, because the Cabal
                -- version is too early.  This is a good, because while all the
                -- deps were bundled up in buildDepends, we didn't do this for
                -- renamings, so it's not even clear how to get the merged
                -- version.  So just assume that all of them are the default..
                else Map.fromList (map (\(_,pid) ->
                                   (packageName pid, defaultRenaming)) cpds)

    selectSubset :: Package pkg => BuildInfo -> [pkg] -> [pkg]
    selectSubset bi pkgs =
        [ pkg | pkg <- pkgs, packageName pkg `elem` names bi ]

    names bi = [ name | Dependency name _ <- targetBuildDepends bi ]

-- | Given the author-specified re-export declarations from the .cabal file,
-- resolve them to the form that we need for the package database.
--
-- An invariant of the package database is that we always link the re-export
-- directly to its original defining location (rather than indirectly via a
-- chain of re-exporting packages).
--
resolveModuleReexports :: InstalledPackageIndex
                       -> PackageId
                       -> UnitId
                       -> [InstalledPackageInfo]
                       -> Library
                       -> Either [(ModuleReexport, String)] -- errors
                                 [Installed.ExposedModule] -- ok
resolveModuleReexports installedPackages srcpkgid key externalPkgDeps lib =
    case partitionEithers
         (map resolveModuleReexport (PD.reexportedModules lib)) of
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
        | let directDeps = Set.fromList
                           (map Installed.installedUnitId externalPkgDeps)
        , pkg <- PackageIndex.allPackages installedPackages
        , Installed.installedUnitId pkg `Set.member` directDeps
        , let exportingPackageName = packageName pkg
        , exposedModule <- visibleModuleDetails pkg
        ]
     ++ [ (visibleModuleName, [(exportingPackageName, exposedModule)])
        | visibleModuleName <- PD.exposedModules lib
                            ++ otherModules (libBuildInfo lib)
        , let exportingPackageName = packageName srcpkgid
              definingModuleName   = visibleModuleName
              definingPackageId    = key
              originalModule = Installed.OriginalModule definingPackageId
                                                        definingModuleName
              exposedModule  = Installed.ExposedModule visibleModuleName
                                                       (Just originalModule)
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
            Nothing -> return exposedModule {
              Installed.exposedReexport =
                 Just (Installed.OriginalModule
                       (Installed.installedUnitId pkg)
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
          | all (\(_, exposedModule') ->
                  Installed.exposedReexport exposedModule
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
