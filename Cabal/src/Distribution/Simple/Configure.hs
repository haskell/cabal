{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
module Distribution.Simple.Configure
  ( configure
  , configure_setupHooks
  , writePersistBuildConfig
  , getConfigStateFile
  , getPersistBuildConfig
  , checkPersistBuildConfigOutdated
  , tryGetPersistBuildConfig
  , maybeGetPersistBuildConfig
  , findDistPref
  , findDistPrefOrDefault
  , getInternalLibraries
  , computeComponentId
  , computeCompatPackageKey
  , localBuildInfoFile
  , getInstalledPackages
  , getInstalledPackagesMonitorFiles
  , getInstalledPackagesById
  , getPackageDBContents
  , configCompilerEx
  , configCompilerAuxEx
  , computeEffectiveProfiling
  , ccLdOptionsBuildInfo
  , checkForeignDeps
  , interpretPackageDbFlags
  , ConfigStateFileError (..)
  , tryGetConfigStateFile
  , platformDefines
  ) where

import Control.Monad
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack.Configure
import Distribution.Backpack.ConfiguredComponent (newPackageDepsBehaviour)
import Distribution.Backpack.DescribeUnitId
import Distribution.Backpack.Id
import Distribution.Backpack.PreExistingComponent
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Stack
import Distribution.Compiler
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check hiding (doesFileExist)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.PrettyPrint
import Distribution.Simple.BuildTarget
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex, lookupUnitId)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
  ( ProgramDb (..)
  , lookupProgramByName
  , modifyProgramSearchPath
  , prependProgramSearchPath
  , updateConfiguredProgs
  )
import Distribution.Simple.Setup.Common as Setup
import Distribution.Simple.Setup.Config as Setup
import Distribution.Simple.SetupHooks.Internal
  ( ConfigureHooks (..)
  , applyComponentDiffs
  , noConfigureHooks
  )
import qualified Distribution.Simple.SetupHooks.Internal as SetupHooks
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.GivenComponent
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.LogProgress
import Distribution.Utils.NubList
import Distribution.Verbosity
import Distribution.Version

import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.UHC as UHC

import Control.Exception
  ( try
  )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.List
  ( intersect
  , stripPrefix
  , (\\)
  )
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Distribution.Compat.Directory
  ( doesPathExist
  , listDirectory
  )
import Distribution.Compat.Environment (lookupEnv)
import Distribution.Parsec
  ( simpleParsec
  )
import Distribution.Pretty
  ( defaultStyle
  , pretty
  , prettyShow
  )
import Distribution.Simple.Errors
import Distribution.Types.AnnotatedId
import Distribution.Utils.Path
import Distribution.Utils.Structured (structuredDecodeOrFailIO, structuredEncode)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeFile
  )
import System.FilePath
  ( isAbsolute
  )
import System.IO
  ( hClose
  , hPutStrLn
  )
import qualified System.Info
  ( compilerName
  , compilerVersion
  )
import Text.PrettyPrint
  ( Doc
  , char
  , hsep
  , quotes
  , renderStyle
  , text
  , ($+$)
  )

import qualified Data.Maybe as M
import qualified Data.Set as Set
import qualified Distribution.Compat.NonEmptySet as NES

type UseExternalInternalDeps = Bool

-- | The errors that can be thrown when reading the @setup-config@ file.
data ConfigStateFileError
  = -- | No header found.
    ConfigStateFileNoHeader
  | -- | Incorrect header.
    ConfigStateFileBadHeader
  | -- | Cannot parse file contents.
    ConfigStateFileNoParse
  | -- | No file!
    ConfigStateFileMissing
      { cfgStateFileErrorCwd :: Maybe (SymbolicPath CWD (Dir Pkg))
      , cfgStateFileErrorFile :: SymbolicPath Pkg File
      }
  | -- | Mismatched version.
    ConfigStateFileBadVersion
      PackageIdentifier
      PackageIdentifier
      (Either ConfigStateFileError LocalBuildInfo)
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
dispConfigStateFileError ConfigStateFileMissing{} =
  text "Run the 'configure' command first."
dispConfigStateFileError (ConfigStateFileBadVersion oldCabal oldCompiler _) =
  text "Saved package config file is outdated:"
    $+$ badCabal
    $+$ badCompiler
    $+$ text "Re-run the 'configure' command."
  where
    badCabal =
      text "• the Cabal version changed from"
        <+> pretty oldCabal
        <+> "to"
        <+> pretty currentCabalId
    badCompiler
      | oldCompiler == currentCompilerId = mempty
      | otherwise =
          text "• the compiler changed from"
            <+> pretty oldCompiler
            <+> "to"
            <+> pretty currentCompilerId

instance Show ConfigStateFileError where
  show = renderStyle defaultStyle . dispConfigStateFileError

instance Exception ConfigStateFileError

-- | Read the 'localBuildInfoFile'.  Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getConfigStateFile
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg File
  -- ^ The file path of the @setup-config@ file.
  -> IO LocalBuildInfo
getConfigStateFile mbWorkDir setupConfigFile = do
  let filename = interpretSymbolicPath mbWorkDir setupConfigFile
  exists <- doesFileExist filename
  unless exists $ throwIO $ ConfigStateFileMissing mbWorkDir setupConfigFile
  -- Read the config file into a strict ByteString to avoid problems with
  -- lazy I/O, then convert to lazy because the binary package needs that.
  contents <- BS.readFile filename
  let (header, body) = BLC8.span (/= '\n') (BLC8.fromChunks [contents])

  (cabalId, compId) <- parseHeader header

  let getStoredValue = do
        result <- structuredDecodeOrFailIO (BLC8.tail body)
        case result of
          Left _ -> throwIO ConfigStateFileNoParse
          Right x -> return x
      deferErrorIfBadVersion act
        | cabalId /= currentCabalId = do
            eResult <- try act
            throwIO $ ConfigStateFileBadVersion cabalId compId eResult
        | otherwise = act
  deferErrorIfBadVersion getStoredValue
  where
    _ = callStack -- TODO: attach call stack to exception

-- | Read the 'localBuildInfoFile', returning either an error or the local build
-- info.
tryGetConfigStateFile
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory.
  -> SymbolicPath Pkg File
  -- ^ The file path of the @setup-config@ file.
  -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetConfigStateFile mbWorkDir = try . getConfigStateFile mbWorkDir

-- | Try to read the 'localBuildInfoFile'.
tryGetPersistBuildConfig
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory.
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ The @dist@ directory path.
  -> IO (Either ConfigStateFileError LocalBuildInfo)
tryGetPersistBuildConfig mbWorkDir = try . getPersistBuildConfig mbWorkDir

-- | Read the 'localBuildInfoFile'. Throw an exception if the file is
-- missing, if the file cannot be read, or if the file was created by an older
-- version of Cabal.
getPersistBuildConfig
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory.
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ The @dist@ directory path.
  -> IO LocalBuildInfo
getPersistBuildConfig mbWorkDir distPref =
  getConfigStateFile mbWorkDir $ localBuildInfoFile distPref

-- | Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory.
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ The @dist@ directory path.
  -> IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig mbWorkDir =
  liftM (either (const Nothing) Just) . tryGetPersistBuildConfig mbWorkDir

-- | After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -- ^ Working directory
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ The @dist@ directory path.
  -> LocalBuildInfo
  -- ^ The 'LocalBuildInfo' to write.
  -> IO ()
writePersistBuildConfig mbWorkDir distPref lbi = do
  createDirectoryIfMissing False (i distPref)
  writeFileAtomic (i $ localBuildInfoFile distPref) $
    BLC8.unlines [showHeader pkgId, structuredEncode lbi]
  where
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path
    pkgId = localPackage lbi

-- | Identifier of the current Cabal package.
currentCabalId :: PackageIdentifier
currentCabalId = PackageIdentifier (mkPackageName "Cabal") cabalVersion

-- | Identifier of the current compiler package.
currentCompilerId :: PackageIdentifier
currentCompilerId =
  PackageIdentifier
    (mkPackageName System.Info.compilerName)
    (mkVersion' System.Info.compilerVersion)

-- | Parse the @setup-config@ file header, returning the package identifiers
-- for Cabal and the compiler.
parseHeader
  :: ByteString
  -- ^ The file contents.
  -> IO (PackageIdentifier, PackageIdentifier)
parseHeader header = case BLC8.words header of
  [ "Saved"
    , "package"
    , "config"
    , "for"
    , pkgId
    , "written"
    , "by"
    , cabalId
    , "using"
    , compId
    ] ->
      maybe (throwIO ConfigStateFileBadHeader) return $ do
        _ <- simpleParsec (fromUTF8LBS pkgId) :: Maybe PackageIdentifier
        cabalId' <- simpleParsec (BLC8.unpack cabalId)
        compId' <- simpleParsec (BLC8.unpack compId)
        return (cabalId', compId')
  _ -> throwIO ConfigStateFileNoHeader

-- | Generate the @setup-config@ file header.
showHeader
  :: PackageIdentifier
  -- ^ The processed package.
  -> ByteString
showHeader pkgId =
  BLC8.unwords
    [ "Saved"
    , "package"
    , "config"
    , "for"
    , toUTF8LBS $ prettyShow pkgId
    , "written"
    , "by"
    , BLC8.pack $ prettyShow currentCabalId
    , "using"
    , BLC8.pack $ prettyShow currentCompilerId
    ]

-- | Check that localBuildInfoFile is up-to-date with respect to the
-- .cabal file.
checkPersistBuildConfigOutdated
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> SymbolicPath Pkg File
  -> IO Bool
checkPersistBuildConfigOutdated mbWorkDir distPref pkg_descr_file =
  i pkg_descr_file `moreRecentFile` i (localBuildInfoFile distPref)
  where
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path

-- | Get the path of @dist\/setup-config@.
localBuildInfoFile
  :: SymbolicPath Pkg (Dir Dist)
  -- ^ The @dist@ directory path.
  -> SymbolicPath Pkg File
localBuildInfoFile distPref = distPref </> makeRelativePathEx "setup-config"

-- -----------------------------------------------------------------------------

-- * Configuration

-- -----------------------------------------------------------------------------

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or the default prefix.
findDistPref
  :: SymbolicPath Pkg (Dir Dist)
  -- ^ default \"dist\" prefix
  -> Setup.Flag (SymbolicPath Pkg (Dir Dist))
  -- ^ override \"dist\" prefix
  -> IO (SymbolicPath Pkg (Dir Dist))
findDistPref defDistPref overrideDistPref = do
  envDistPref <- liftM parseEnvDistPref (lookupEnv "CABAL_BUILDDIR")
  return $ fromFlagOrDefault defDistPref (mappend envDistPref overrideDistPref)
  where
    parseEnvDistPref env =
      case env of
        Just distPref | not (null distPref) -> toFlag $ makeSymbolicPath distPref
        _ -> NoFlag

-- | Return the \"dist/\" prefix, or the default prefix. The prefix is taken
-- from (in order of highest to lowest preference) the override prefix, the
-- \"CABAL_BUILDDIR\" environment variable, or 'defaultDistPref' is used. Call
-- this function to resolve a @*DistPref@ flag whenever it is not known to be
-- set. (The @*DistPref@ flags are always set to a definite value before
-- invoking 'UserHooks'.)
findDistPrefOrDefault
  :: Setup.Flag (SymbolicPath Pkg (Dir Dist))
  -- ^ override \"dist\" prefix
  -> IO (SymbolicPath Pkg (Dir Dist))
findDistPrefOrDefault = findDistPref defaultDistPref

-- | Perform the \"@.\/setup configure@\" action.
--  Returns the @.setup-config@ file.
configure
  :: (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
configure = configure_setupHooks noConfigureHooks

configure_setupHooks
  :: ConfigureHooks
  -> (GenericPackageDescription, HookedBuildInfo)
  -> ConfigFlags
  -> IO LocalBuildInfo
configure_setupHooks
  (ConfigureHooks{preConfPackageHook, postConfPackageHook, preConfComponentHook})
  (g_pkg_descr, hookedBuildInfo)
  cfg = do
    -- Cabal pre-configure
    let verbosity = fromFlag (configVerbosity cfg)
        distPref = fromFlag $ configDistPref cfg
        mbWorkDir = flagToMaybe $ configWorkingDir cfg
    (lbc0, comp, platform, enabledComps) <- preConfigurePackage cfg g_pkg_descr

    -- Package-wide pre-configure hook
    lbc1 <-
      case preConfPackageHook of
        Nothing -> return lbc0
        Just pre_conf -> do
          let programDb0 = LBC.withPrograms lbc0
              programDb0' = programDb0{unconfiguredProgs = Map.empty}
              input =
                SetupHooks.PreConfPackageInputs
                  { SetupHooks.configFlags = cfg
                  , SetupHooks.localBuildConfig = lbc0{LBC.withPrograms = programDb0'}
                  , -- Unconfigured programs are not supplied to the hook,
                    -- as these cannot be passed over a serialisation boundary
                    -- (see the "Binary ProgramDb" instance).
                    SetupHooks.compiler = comp
                  , SetupHooks.platform = platform
                  }
          SetupHooks.PreConfPackageOutputs
            { SetupHooks.buildOptions = opts1
            , SetupHooks.extraConfiguredProgs = progs1
            } <-
            pre_conf input
          -- The package-wide pre-configure hook returns BuildOptions that
          -- overrides the one it was passed in, as well as an update to
          -- the ProgramDb in the form of new configured programs to add
          -- to the program database.
          return $
            lbc0
              { LBC.withBuildOptions = opts1
              , LBC.withPrograms =
                  updateConfiguredProgs
                    (`Map.union` progs1)
                    programDb0
              }

    -- Cabal package-wide configure
    (lbc2, pbd2, pkg_info) <-
      finalizeAndConfigurePackage cfg lbc1 g_pkg_descr comp platform enabledComps

    -- Package-wide post-configure hook
    for_ postConfPackageHook $ \postConfPkg -> do
      let input =
            SetupHooks.PostConfPackageInputs
              { SetupHooks.localBuildConfig = lbc2
              , SetupHooks.packageBuildDescr = pbd2
              }
      postConfPkg input

    -- Per-component pre-configure hook
    pkg_descr <- do
      let pkg_descr2 = LBC.localPkgDescr pbd2
      applyComponentDiffs
        verbosity
        ( \c -> for preConfComponentHook $ \computeDiff -> do
            let input =
                  SetupHooks.PreConfComponentInputs
                    { SetupHooks.localBuildConfig = lbc2
                    , SetupHooks.packageBuildDescr = pbd2
                    , SetupHooks.component = c
                    }
            SetupHooks.PreConfComponentOutputs
              { SetupHooks.componentDiff = diff
              } <-
              computeDiff input
            return diff
        )
        pkg_descr2
    let pbd3 = pbd2{LBC.localPkgDescr = pkg_descr}

    -- Cabal per-component configure
    externalPkgDeps <- finalCheckPackage g_pkg_descr pbd3 hookedBuildInfo pkg_info
    lbi <- configureComponents lbc2 pbd3 pkg_info externalPkgDeps

    writePersistBuildConfig mbWorkDir distPref lbi

    return lbi

preConfigurePackage
  :: ConfigFlags
  -> GenericPackageDescription
  -> IO (LBC.LocalBuildConfig, Compiler, Platform, ComponentRequestedSpec)
preConfigurePackage cfg g_pkg_descr = do
  let verbosity = fromFlag $ configVerbosity cfg

  -- Determine the component we are configuring, if a user specified
  -- one on the command line.  We use a fake, flattened version of
  -- the package since at this point, we're not really sure what
  -- components we *can* configure.  @Nothing@ means that we should
  -- configure everything (the old behavior).
  (mb_cname :: Maybe ComponentName) <- do
    let flat_pkg_descr = flattenPackageDescription g_pkg_descr
        targets0 = configTargets cfg
    targets <- readBuildTargets verbosity flat_pkg_descr targets0
    -- TODO: bleat if you use the module/file syntax
    let targets' = [cname | BuildTargetComponent cname <- targets]
    case targets' of
      _ | null targets0 -> return Nothing
      [cname] -> return (Just cname)
      [] -> dieWithException verbosity NoValidComponent
      _ -> dieWithException verbosity ConfigureEitherSingleOrAll

  case mb_cname of
    Nothing -> setupMessage verbosity "Configuring" (packageId g_pkg_descr)
    Just cname ->
      setupMessage'
        verbosity
        "Configuring"
        (packageId g_pkg_descr)
        cname
        (Just (configInstantiateWith cfg))

  -- configCID is only valid for per-component configure
  when (isJust (flagToMaybe (configCID cfg)) && isNothing mb_cname) $
    dieWithException verbosity ConfigCIDValidForPreComponent

  -- Make a data structure describing what components are enabled.
  let enabled :: ComponentRequestedSpec
      enabled = case mb_cname of
        Just cname -> OneComponentRequestedSpec cname
        Nothing ->
          ComponentRequestedSpec
            { -- The flag name (@--enable-tests@) is a
              -- little bit of a misnomer, because
              -- just passing this flag won't
              -- "enable", in our internal
              -- nomenclature; it's just a request; a
              -- @buildable: False@ might make it
              -- not possible to enable.
              testsRequested = fromFlag (configTests cfg)
            , benchmarksRequested =
                fromFlag (configBenchmarks cfg)
            }
  -- Some sanity checks related to enabling components.
  when
    ( isJust mb_cname
        && (fromFlag (configTests cfg) || fromFlag (configBenchmarks cfg))
    )
    $ dieWithException verbosity SanityCheckForEnableComponents

  checkDeprecatedFlags verbosity cfg
  checkExactConfiguration verbosity g_pkg_descr cfg

  programDbPre <- mkProgramDb cfg (configPrograms cfg)
  -- comp:            the compiler we're building with
  -- compPlatform:    the platform we're building for
  -- programDb:  location and args of all programs we're
  --                  building with
  ( comp :: Compiler
    , compPlatform :: Platform
    , programDb00 :: ProgramDb
    ) <-
    configCompilerEx
      (flagToMaybe (configHcFlavor cfg))
      (flagToMaybe (configHcPath cfg))
      (flagToMaybe (configHcPkg cfg))
      programDbPre
      (lessVerbose verbosity)

  -- Where to build the package
  let builddir :: SymbolicPath Pkg (Dir Build) -- e.g. dist/build
      builddir = setupFlagsBuildDir $ configCommonFlags cfg
      mbWorkDir = flagToMaybe $ configWorkingDir cfg
  -- NB: create this directory now so that all configure hooks get
  -- to see it. (In practice, the Configure build-type needs it before
  -- the postConfPackageHook runs.)
  createDirectoryIfMissingVerbose (lessVerbose verbosity) True $
    interpretSymbolicPath mbWorkDir builddir

  lbc <- computeLocalBuildConfig cfg comp programDb00
  return (lbc, comp, compPlatform, enabled)

computeLocalBuildConfig
  :: ConfigFlags
  -> Compiler
  -> ProgramDb
  -> IO LBC.LocalBuildConfig
computeLocalBuildConfig cfg comp programDb = do
  let common = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity common
  -- Decide if we're going to compile with split sections.
  split_sections :: Bool <-
    if not (fromFlag $ configSplitSections cfg)
      then return False
      else case compilerFlavor comp of
        GHC
          | compilerVersion comp >= mkVersion [8, 0] ->
              return True
        GHCJS ->
          return True
        _ -> do
          warn
            verbosity
            ( "this compiler does not support "
                ++ "--enable-split-sections; ignoring"
            )
          return False

  -- Decide if we're going to compile with split objects.
  split_objs :: Bool <-
    if not (fromFlag $ configSplitObjs cfg)
      then return False
      else case compilerFlavor comp of
        _ | split_sections ->
          do
            warn
              verbosity
              ( "--enable-split-sections and "
                  ++ "--enable-split-objs are mutually "
                  ++ "exclusive; ignoring the latter"
              )
            return False
        GHC ->
          return True
        GHCJS ->
          return True
        _ -> do
          warn
            verbosity
            ( "this compiler does not support "
                ++ "--enable-split-objs; ignoring"
            )
          return False

  -- Basically yes/no/unknown.
  let linkerSupportsRelocations :: Maybe Bool
      linkerSupportsRelocations =
        case lookupProgramByName "ld" programDb of
          Nothing -> Nothing
          Just ld ->
            case Map.lookup "Supports relocatable output" $ programProperties ld of
              Just "YES" -> Just True
              Just "NO" -> Just False
              _other -> Nothing
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

  withGHCiLib_ <-
    case fromFlagOrDefault ghciLibByDefault (configGHCiLib cfg) of
      -- NOTE: If linkerSupportsRelocations is Nothing this may still fail if the
      -- linker does not support -r.
      True | not (fromMaybe True linkerSupportsRelocations) -> do
        warn verbosity $
          "--enable-library-for-ghci is not supported with the current"
            ++ "  linker; ignoring..."
        return False
      v -> return v

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

      withStaticLib_ =
        -- build a static library (all dependent libraries rolled
        -- into a huge .a archive) via GHCs -staticlib flag.
        fromFlagOrDefault False $ configStaticLib cfg

      withDynExe_ = fromFlag $ configDynExe cfg

      withFullyStaticExe_ = fromFlag $ configFullyStaticExe cfg

  when (withDynExe_ && not withSharedLib_) $
    warn verbosity $
      "Executables will use dynamic linking, but a shared library "
        ++ "is not being built. Linking will fail if any executables "
        ++ "depend on the library."

  setProfiling <- configureProfiling verbosity cfg comp

  setCoverage <- configureCoverage verbosity cfg comp

  -- Turn off library and executable stripping when `debug-info` is set
  -- to anything other than zero.
  let
    strip_libexe s f =
      let defaultStrip = fromFlagOrDefault True (f cfg)
       in case fromFlag (configDebugInfo cfg) of
            NoDebugInfo -> return defaultStrip
            _ -> case f cfg of
              Flag True -> do
                warn verbosity $
                  "Setting debug-info implies "
                    ++ s
                    ++ "-stripping: False"
                return False
              _ -> return False

  strip_lib <- strip_libexe "library" configStripLibs
  strip_exe <- strip_libexe "executable" configStripExes

  let buildOptions =
        setCoverage . setProfiling $
          LBC.BuildOptions
            { withVanillaLib = fromFlag $ configVanillaLib cfg
            , withSharedLib = withSharedLib_
            , withStaticLib = withStaticLib_
            , withDynExe = withDynExe_
            , withFullyStaticExe = withFullyStaticExe_
            , withProfLib = False
            , withProfLibDetail = ProfDetailNone
            , withProfExe = False
            , withProfExeDetail = ProfDetailNone
            , withOptimization = fromFlag $ configOptimization cfg
            , withDebugInfo = fromFlag $ configDebugInfo cfg
            , withGHCiLib = withGHCiLib_
            , splitSections = split_sections
            , splitObjs = split_objs
            , stripExes = strip_exe
            , stripLibs = strip_lib
            , exeCoverage = False
            , libCoverage = False
            , relocatable = fromFlagOrDefault False $ configRelocatable cfg
            }

  return $
    LBC.LocalBuildConfig
      { extraConfigArgs = [] -- Currently configure does not
      -- take extra args, but if it
      -- did they would go here.
      , withPrograms = programDb
      , withBuildOptions = buildOptions
      }

data PackageInfo = PackageInfo
  { internalPackageSet :: Set LibraryName
  , promisedDepsSet :: Map (PackageName, ComponentName) ComponentId
  , installedPackageSet :: InstalledPackageIndex
  , requiredDepsMap :: Map (PackageName, ComponentName) InstalledPackageInfo
  }

configurePackage
  :: ConfigFlags
  -> LBC.LocalBuildConfig
  -> PackageDescription
  -> FlagAssignment
  -> ComponentRequestedSpec
  -> Compiler
  -> Platform
  -> ProgramDb
  -> PackageDBStack
  -> IO (LBC.LocalBuildConfig, LBC.PackageBuildDescr)
configurePackage cfg lbc0 pkg_descr00 flags enabled comp platform programDb0 packageDbs = do
  let common = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity common

      -- add extra include/lib dirs as specified in cfg
      pkg_descr0 = addExtraIncludeLibDirsFromConfigFlags pkg_descr00 cfg
  -- TODO: it is not clear whether this adding these dirs is necessary
  -- when we are directly stating from a PackageDescription (e.g. when
  -- cabal-install has determined a PackageDescription, instead of rediscovering
  -- when working with a GenericPackageDescription).
  -- Could this function call be moved to the end of finalizeAndConfigurePackage
  -- right before calling configurePackage?

  -- Configure certain external build tools, see below for which ones.
  let requiredBuildTools = do
        bi <- enabledBuildInfos pkg_descr0 enabled
        -- First, we collect any tool dep that we know is external. This is,
        -- in practice:
        --
        -- 1. `build-tools` entries on the whitelist
        --
        -- 2. `build-tool-depends` that aren't from the current package.
        let externBuildToolDeps =
              [ LegacyExeDependency (unUnqualComponentName eName) versionRange
              | buildTool@(ExeDependency _ eName versionRange) <-
                  getAllToolDependencies pkg_descr0 bi
              , not $ isInternal pkg_descr0 buildTool
              ]
        -- Second, we collect any build-tools entry we don't know how to
        -- desugar. We'll never have any idea how to build them, so we just
        -- hope they are already on the PATH.
        let unknownBuildTools =
              [ buildTool
              | buildTool <- buildTools bi
              , Nothing == desugarBuildTool pkg_descr0 buildTool
              ]
        externBuildToolDeps ++ unknownBuildTools

  programDb1 <-
    configureAllKnownPrograms (lessVerbose verbosity) programDb0
      >>= configureRequiredPrograms verbosity requiredBuildTools

  (pkg_descr2, programDb2) <-
    configurePkgconfigPackages verbosity pkg_descr0 programDb1 enabled

  let use_external_internal_deps =
        case enabled of
          OneComponentRequestedSpec{} -> True
          ComponentRequestedSpec{} -> False

  -- Compute installation directory templates, based on user
  -- configuration.
  --
  -- TODO: Move this into a helper function.
  defaultDirs :: InstallDirTemplates <-
    defaultInstallDirs'
      use_external_internal_deps
      (compilerFlavor comp)
      (fromFlag (configUserInstall cfg))
      (hasLibs pkg_descr2)
  let
    installDirs =
      combineInstallDirs
        fromFlagOrDefault
        defaultDirs
        (configInstallDirs cfg)
    lbc = lbc0{LBC.withPrograms = programDb2}
    pbd =
      LBC.PackageBuildDescr
        { configFlags = cfg
        , flagAssignment = flags
        , componentEnabledSpec = enabled
        , compiler = comp
        , hostPlatform = platform
        , localPkgDescr = pkg_descr2
        , installDirTemplates = installDirs
        , withPackageDB = packageDbs
        , pkgDescrFile = Nothing
        , extraCoverageFor = []
        }

  debug verbosity $
    "Finalized package description:\n"
      ++ showPackageDescription pkg_descr2

  return (lbc, pbd)

finalizeAndConfigurePackage
  :: ConfigFlags
  -> LBC.LocalBuildConfig
  -> GenericPackageDescription
  -> Compiler
  -> Platform
  -> ComponentRequestedSpec
  -> IO (LBC.LocalBuildConfig, LBC.PackageBuildDescr, PackageInfo)
finalizeAndConfigurePackage cfg lbc0 g_pkg_descr comp platform enabled = do
  let common = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity common
      mbWorkDir = flagToMaybe $ setupWorkingDir common

  let programDb0 = LBC.withPrograms lbc0
      -- What package database(s) to use
      packageDbs :: PackageDBStack
      packageDbs =
        interpretPackageDbFlags
          (fromFlag (configUserInstall cfg))
          (configPackageDBs cfg)

  -- The InstalledPackageIndex of all installed packages
  installedPackageSet :: InstalledPackageIndex <-
    getInstalledPackages
      (lessVerbose verbosity)
      comp
      mbWorkDir
      packageDbs
      programDb0

  -- The set of package names which are "shadowed" by internal
  -- packages, and which component they map to
  let internalPackageSet :: Set LibraryName
      internalPackageSet = getInternalLibraries g_pkg_descr

  -- Some sanity checks related to dynamic/static linking.
  when (fromFlag (configDynExe cfg) && fromFlag (configFullyStaticExe cfg)) $
    dieWithException verbosity SanityCheckForDynamicStaticLinking

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
  ( allConstraints :: [PackageVersionConstraint]
    , requiredDepsMap :: Map (PackageName, ComponentName) InstalledPackageInfo
    ) <-
    either (dieWithException verbosity) return $
      combinedConstraints
        (configConstraints cfg)
        (configDependencies cfg)
        installedPackageSet

  let
    promisedDepsSet = mkPromisedDepsSet (configPromisedDependencies cfg)
    pkg_info =
      PackageInfo
        { internalPackageSet
        , promisedDepsSet
        , installedPackageSet
        , requiredDepsMap
        }

  -- pkg_descr:   The resolved package description, that does not contain any
  --              conditionals, because we have an assignment for
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
  let use_external_internal_deps = case enabled of
        OneComponentRequestedSpec{} -> True
        ComponentRequestedSpec{} -> False
  ( pkg_descr0 :: PackageDescription
    , flags :: FlagAssignment
    ) <-
    configureFinalizedPackage
      verbosity
      cfg
      enabled
      allConstraints
      ( dependencySatisfiable
          use_external_internal_deps
          (fromFlagOrDefault False (configExactConfiguration cfg))
          (fromFlagOrDefault False (configAllowDependingOnPrivateLibs cfg))
          (packageName g_pkg_descr)
          installedPackageSet
          internalPackageSet
          promisedDepsSet
          requiredDepsMap
      )
      comp
      platform
      g_pkg_descr

  (lbc, pbd) <-
    configurePackage
      cfg
      lbc0
      pkg_descr0
      flags
      enabled
      comp
      platform
      programDb0
      packageDbs
  return (lbc, pbd, pkg_info)

addExtraIncludeLibDirsFromConfigFlags
  :: PackageDescription -> ConfigFlags -> PackageDescription
addExtraIncludeLibDirsFromConfigFlags pkg_descr cfg =
  let extraBi =
        mempty
          { extraLibDirs = configExtraLibDirs cfg
          , extraLibDirsStatic = configExtraLibDirsStatic cfg
          , extraFrameworkDirs = configExtraFrameworkDirs cfg
          , includeDirs = configExtraIncludeDirs cfg
          }
      modifyLib l =
        l
          { libBuildInfo =
              libBuildInfo l
                `mappend` extraBi
          }
      modifyExecutable e =
        e
          { buildInfo =
              buildInfo e
                `mappend` extraBi
          }
      modifyForeignLib f =
        f
          { foreignLibBuildInfo =
              foreignLibBuildInfo f
                `mappend` extraBi
          }
      modifyTestsuite t =
        t
          { testBuildInfo =
              testBuildInfo t
                `mappend` extraBi
          }
      modifyBenchmark b =
        b
          { benchmarkBuildInfo =
              benchmarkBuildInfo b
                `mappend` extraBi
          }
   in pkg_descr
        { library = modifyLib `fmap` library pkg_descr
        , subLibraries = modifyLib `map` subLibraries pkg_descr
        , executables = modifyExecutable `map` executables pkg_descr
        , foreignLibs = modifyForeignLib `map` foreignLibs pkg_descr
        , testSuites = modifyTestsuite `map` testSuites pkg_descr
        , benchmarks = modifyBenchmark `map` benchmarks pkg_descr
        }

finalCheckPackage
  :: GenericPackageDescription
  -> LBC.PackageBuildDescr
  -> HookedBuildInfo
  -> PackageInfo
  -> IO ([PreExistingComponent], [PromisedComponent])
finalCheckPackage
  g_pkg_descr
  ( LBC.PackageBuildDescr
      { configFlags = cfg
      , localPkgDescr = pkg_descr
      , compiler = comp
      , hostPlatform = compPlatform
      , componentEnabledSpec = enabled
      }
    )
  hookedBuildInfo
  (PackageInfo{internalPackageSet, promisedDepsSet, installedPackageSet, requiredDepsMap}) =
    do
      let common = configCommonFlags cfg
          verbosity = fromFlag $ setupVerbosity common
          cabalFileDir = packageRoot common
          use_external_internal_deps =
            case enabled of
              OneComponentRequestedSpec{} -> True
              ComponentRequestedSpec{} -> False

      checkCompilerProblems verbosity comp pkg_descr enabled
      checkPackageProblems
        verbosity
        cabalFileDir
        g_pkg_descr
        (updatePackageDescription hookedBuildInfo pkg_descr)
      -- NB: we apply the HookedBuildInfo to check it is valid,
      -- but we don't propagate it.
      -- Other UserHooks must separately return it again, and we
      -- will re-apply it each time.

      -- Check languages and extensions
      -- TODO: Move this into a helper function.
      let langlist =
            nub $
              catMaybes $
                map
                  defaultLanguage
                  (enabledBuildInfos pkg_descr enabled)
      let langs = unsupportedLanguages comp langlist
      when (not (null langs)) $
        dieWithException verbosity $
          UnsupportedLanguages (packageId g_pkg_descr) (compilerId comp) (map prettyShow langs)
      let extlist =
            nub $
              concatMap
                allExtensions
                (enabledBuildInfos pkg_descr enabled)
      let exts = unsupportedExtensions comp extlist
      when (not (null exts)) $
        dieWithException verbosity $
          UnsupportedLanguageExtension (packageId g_pkg_descr) (compilerId comp) (map prettyShow exts)

      -- Check foreign library build requirements
      let flibs = [flib | CFLib flib <- enabledComponents pkg_descr enabled]
      let unsupportedFLibs = unsupportedForeignLibs comp compPlatform flibs
      when (not (null unsupportedFLibs)) $
        dieWithException verbosity $
          CantFindForeignLibraries unsupportedFLibs

      -- The list of 'InstalledPackageInfo' recording the selected
      -- dependencies on external packages.
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
      --
      -- In the presence of Backpack, these package dependencies are
      -- NOT complete: they only ever include the INDEFINITE
      -- dependencies.  After we apply an instantiation, we'll get
      -- definite references which constitute extra dependencies.
      -- (Why not have cabal-install pass these in explicitly?
      -- For one it's deterministic; for two, we need to associate
      -- them with renamings which would require a far more complicated
      -- input scheme than what we have today.)
      configureDependencies
        verbosity
        use_external_internal_deps
        internalPackageSet
        promisedDepsSet
        installedPackageSet
        requiredDepsMap
        pkg_descr
        enabled

configureComponents
  :: LBC.LocalBuildConfig
  -> LBC.PackageBuildDescr
  -> PackageInfo
  -> ([PreExistingComponent], [PromisedComponent])
  -> IO LocalBuildInfo
configureComponents
  lbc@(LBC.LocalBuildConfig{withPrograms = programDb})
  pbd0@( LBC.PackageBuildDescr
          { configFlags = cfg
          , localPkgDescr = pkg_descr
          , compiler = comp
          , componentEnabledSpec = enabled
          }
        )
  (PackageInfo{promisedDepsSet, installedPackageSet})
  externalPkgDeps =
    do
      let common = configCommonFlags cfg
          verbosity = fromFlag $ setupVerbosity common
          use_external_internal_deps =
            case enabled of
              OneComponentRequestedSpec{} -> True
              ComponentRequestedSpec{} -> False

      -- Compute internal component graph
      --
      -- The general idea is that we take a look at all the source level
      -- components (which may build-depends on each other) and form a graph.
      -- From there, we build a ComponentLocalBuildInfo for each of the
      -- components, which lets us actually build each component.
      ( buildComponents :: [ComponentLocalBuildInfo]
        , packageDependsIndex :: InstalledPackageIndex
        ) <-
        runLogProgress verbosity $
          configureComponentLocalBuildInfos
            verbosity
            use_external_internal_deps
            enabled
            (fromFlagOrDefault False (configDeterministic cfg))
            (configIPID cfg)
            (configCID cfg)
            pkg_descr
            externalPkgDeps
            (configConfigurationsFlags cfg)
            (configInstantiateWith cfg)
            installedPackageSet
            comp

      let buildComponentsMap =
            foldl'
              ( \m clbi ->
                  Map.insertWith
                    (++)
                    (componentLocalName clbi)
                    [clbi]
                    m
              )
              Map.empty
              buildComponents

      let cbd =
            LBC.ComponentBuildDescr
              { componentGraph = Graph.fromDistinctList buildComponents
              , componentNameMap = buildComponentsMap
              , promisedPkgs = promisedDepsSet
              , installedPkgs = packageDependsIndex
              }

          -- For whole-package configure, we determine the
          -- extraCoverageFor of the main lib and sub libs here.
          extraCoverageUnitIds = case enabled of
            -- Whole package configure, add package libs
            ComponentRequestedSpec{} -> mapMaybe mbCompUnitId buildComponents
            -- Component configure, no need to do anything
            OneComponentRequestedSpec{} -> []
          mbCompUnitId LibComponentLocalBuildInfo{componentUnitId} = Just componentUnitId
          mbCompUnitId _ = Nothing

          pbd =
            pbd0
              { LBC.extraCoverageFor = extraCoverageUnitIds
              }

          lbd =
            LBC.LocalBuildDescr
              { packageBuildDescr = pbd
              , componentBuildDescr = cbd
              }

          lbi =
            NewLocalBuildInfo
              { localBuildDescr = lbd
              , localBuildConfig = lbc
              }

      when (LBC.relocatable $ LBC.withBuildOptions lbc) $
        checkRelocatable verbosity pkg_descr lbi

      -- TODO: This is not entirely correct, because the dirs may vary
      -- across libraries/executables
      let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
          relative = prefixRelativeInstallDirs (packageId pkg_descr) lbi

      -- PKGROOT: allowing ${pkgroot} to be passed as --prefix to
      -- cabal configure, is only a hidden option. It allows packages
      -- to be relocatable with their package database.  This however
      -- breaks when the Paths_* or other includes are used that
      -- contain hard coded paths. This is still an open TODO.
      --
      -- Allowing ${pkgroot} here, however requires less custom hooks
      -- in scripts that *really* want ${pkgroot}. See haskell/cabal/#4872
      unless
        ( isAbsolute (prefix dirs)
            || "${pkgroot}" `isPrefixOf` prefix dirs
        )
        $ dieWithException verbosity
        $ ExpectedAbsoluteDirectory (prefix dirs)

      when ("${pkgroot}" `isPrefixOf` prefix dirs) $
        warn verbosity $
          "Using ${pkgroot} in prefix "
            ++ prefix dirs
            ++ " will not work if you rely on the Path_* module "
            ++ " or other hard coded paths.  Cabal does not yet "
            ++ " support fully relocatable builds! "
            ++ " See #462 #2302 #2994 #3305 #3473 #3586 #3909"
            ++ " #4097 #4291 #4872"

      info verbosity $
        "Using "
          ++ prettyShow currentCabalId
          ++ " compiled by "
          ++ prettyShow currentCompilerId
      info verbosity $ "Using compiler: " ++ showCompilerId comp
      info verbosity $ "Using install prefix: " ++ prefix dirs

      let dirinfo name dir isPrefixRelative =
            info verbosity $ name ++ " installed in: " ++ dir ++ relNote
            where
              relNote = case buildOS of
                Windows
                  | not (hasLibs pkg_descr)
                      && isNothing isPrefixRelative ->
                      "  (fixed location)"
                _ -> ""

      dirinfo "Executables" (bindir dirs) (bindir relative)
      dirinfo "Libraries" (libdir dirs) (libdir relative)
      dirinfo "Dynamic Libraries" (dynlibdir dirs) (dynlibdir relative)
      dirinfo "Private executables" (libexecdir dirs) (libexecdir relative)
      dirinfo "Data files" (datadir dirs) (datadir relative)
      dirinfo "Documentation" (docdir dirs) (docdir relative)
      dirinfo "Configuration files" (sysconfdir dirs) (sysconfdir relative)

      sequence_
        [ reportProgram verbosity prog configuredProg
        | (prog, configuredProg) <- knownPrograms programDb
        ]

      return lbi

mkPromisedDepsSet :: [GivenComponent] -> Map (PackageName, ComponentName) ComponentId
mkPromisedDepsSet comps = Map.fromList [((pn, CLibName ln), cid) | GivenComponent pn ln cid <- comps]

-- | Adds the extra program paths from the flags provided to @configure@ as
-- well as specified locations for certain known programs and their default
-- arguments.
mkProgramDb :: ConfigFlags -> ProgramDb -> IO ProgramDb
mkProgramDb cfg initialProgramDb = do
  programDb <-
    modifyProgramSearchPath (getProgramSearchPath initialProgramDb ++) -- We need to have the paths to programs installed by build-tool-depends before all other paths
      <$> prependProgramSearchPath (fromFlagOrDefault normal (configVerbosity cfg)) searchpath [] initialProgramDb
  pure
    . userSpecifyArgss (configProgramArgs cfg)
    . userSpecifyPaths (configProgramPaths cfg)
    $ programDb
  where
    searchpath = fromNubList (configProgramPathExtra cfg)

-- Note. We try as much as possible to _prepend_ rather than postpend the extra-prog-path
-- so that we can override the system path. However, in a v2-build, at this point, the "system" path
-- has already been extended by both the built-tools-depends paths, as well as the program-path-extra
-- so for v2 builds adding it again is entirely unnecessary. However, it needs to get added again _anyway_
-- so as to take effect for v1 builds or standalone calls to Setup.hs
-- In this instance, the lesser evil is to not allow it to override the system path.

-- -----------------------------------------------------------------------------
-- Helper functions for configure

-- | Check if the user used any deprecated flags.
checkDeprecatedFlags :: Verbosity -> ConfigFlags -> IO ()
checkDeprecatedFlags verbosity cfg = do
  unless (configProfExe cfg == NoFlag) $ do
    let enable
          | fromFlag (configProfExe cfg) = "enable"
          | otherwise = "disable"
    warn
      verbosity
      ( "The flag --"
          ++ enable
          ++ "-executable-profiling is deprecated. "
          ++ "Please use --"
          ++ enable
          ++ "-profiling instead."
      )

  unless (configLibCoverage cfg == NoFlag) $ do
    let enable
          | fromFlag (configLibCoverage cfg) = "enable"
          | otherwise = "disable"
    warn
      verbosity
      ( "The flag --"
          ++ enable
          ++ "-library-coverage is deprecated. "
          ++ "Please use --"
          ++ enable
          ++ "-coverage instead."
      )

-- | Sanity check: if '--exact-configuration' was given, ensure that the
-- complete flag assignment was specified on the command line.
checkExactConfiguration
  :: Verbosity -> GenericPackageDescription -> ConfigFlags -> IO ()
checkExactConfiguration verbosity pkg_descr0 cfg =
  when (fromFlagOrDefault False (configExactConfiguration cfg)) $ do
    let cmdlineFlags = map fst (unFlagAssignment (configConfigurationsFlags cfg))
        allFlags = map flagName . genPackageFlags $ pkg_descr0
        diffFlags = allFlags \\ cmdlineFlags
    when (not . null $ diffFlags) $
      dieWithException verbosity $
        FlagsNotSpecified diffFlags

-- | Create a PackageIndex that makes *any libraries that might be*
-- defined internally to this package look like installed packages, in
-- case an executable should refer to any of them as dependencies.
--
-- It must be *any libraries that might be* defined rather than the
-- actual definitions, because these depend on conditionals in the .cabal
-- file, and we haven't resolved them yet.  finalizePD
-- does the resolution of conditionals, and it takes internalPackageSet
-- as part of its input.
getInternalLibraries
  :: GenericPackageDescription
  -> Set LibraryName
getInternalLibraries pkg_descr0 =
  -- TODO: some day, executables will be fair game here too!
  let pkg_descr = flattenPackageDescription pkg_descr0
   in Set.fromList (map libName (allLibraries pkg_descr))

-- | Returns true if a dependency is satisfiable.  This function may
-- report a dependency satisfiable even when it is not, but not vice
-- versa. This is to be passed to finalize
dependencySatisfiable
  :: Bool
  -- ^ use external internal deps?
  -> Bool
  -- ^ exact configuration?
  -> Bool
  -- ^ allow depending on private libs?
  -> PackageName
  -> InstalledPackageIndex
  -- ^ installed set
  -> Set LibraryName
  -- ^ library components
  -> Map (PackageName, ComponentName) ComponentId
  -> Map (PackageName, ComponentName) InstalledPackageInfo
  -- ^ required dependencies
  -> (Dependency -> Bool)
dependencySatisfiable
  use_external_internal_deps
  exact_config
  allow_private_deps
  pn
  installedPackageSet
  packageLibraries
  promisedDeps
  requiredDepsMap
  (Dependency depName vr sublibs)
    | exact_config =
        -- When we're given '--exact-configuration', we assume that all
        -- dependencies and flags are exactly specified on the command
        -- line. Thus we only consult the 'requiredDepsMap'. Note that
        -- we're not doing the version range check, so if there's some
        -- dependency that wasn't specified on the command line,
        -- 'finalizePD' will fail.
        -- TODO: mention '--exact-configuration' in the error message
        -- when this fails?
        if isInternalDep && not use_external_internal_deps
          then -- Except for internal deps, when we're NOT per-component mode;
          -- those are just True.
            internalDepSatisfiable
          else -- Backward compatibility for the old sublibrary syntax

            ( sublibs == mainLibSet
                && Map.member
                  ( pn
                  , CLibName $
                      LSubLibName $
                        packageNameToUnqualComponentName depName
                  )
                  requiredDepsMap
            )
              || all visible sublibs
    | isInternalDep =
        if use_external_internal_deps
          then -- When we are doing per-component configure, we now need to
          -- test if the internal dependency is in the index.  This has
          -- DIFFERENT semantics from normal dependency satisfiability.
            internalDepSatisfiableExternally
          else -- If a 'PackageName' is defined by an internal component, the dep is
          -- satisfiable (we're going to build it ourselves)
            internalDepSatisfiable
    | otherwise =
        depSatisfiable
    where
      -- Internal dependency is when dependency is the same as package.
      isInternalDep = pn == depName

      depSatisfiable =
        not . null $ PackageIndex.lookupDependency installedPackageSet depName vr

      internalDepSatisfiable =
        Set.isSubsetOf (NES.toSet sublibs) packageLibraries
      internalDepSatisfiableExternally =
        all (\ln -> not $ null $ PackageIndex.lookupInternalDependency installedPackageSet pn vr ln) sublibs

      -- Check whether a library exists and is visible.
      -- We don't disambiguate between dependency on non-existent or private
      -- library yet, so we just return a bool and later report a generic error.
      visible lib =
        maybe
          False -- Does not even exist (wasn't in the depsMap)
          ( \ipi ->
              IPI.libVisibility ipi == LibraryVisibilityPublic
                -- If the override is enabled, the visibility does
                -- not matter (it's handled externally)
                || allow_private_deps
                -- If it's a library of the same package then it's
                -- always visible.
                -- This is only triggered when passing a component
                -- of the same package as --dependency, such as in:
                -- cabal-testsuite/PackageTests/ConfigureComponent/SubLib/setup-explicit.test.hs
                || pkgName (IPI.sourcePackageId ipi) == pn
          )
          maybeIPI
          -- Don't check if it's visible, we promise to build it before we need it.
          || promised
        where
          maybeIPI = Map.lookup (depName, CLibName lib) requiredDepsMap
          promised = isJust $ Map.lookup (depName, CLibName lib) promisedDeps

-- | Finalize a generic package description.
--
-- The workhorse is 'finalizePD'.
configureFinalizedPackage
  :: Verbosity
  -> ConfigFlags
  -> ComponentRequestedSpec
  -> [PackageVersionConstraint]
  -> (Dependency -> Bool)
  -- ^ tests if a dependency is satisfiable.
  -- Might say it's satisfiable even when not.
  -> Compiler
  -> Platform
  -> GenericPackageDescription
  -> IO (PackageDescription, FlagAssignment)
configureFinalizedPackage
  verbosity
  cfg
  enabled
  allConstraints
  satisfies
  comp
  compPlatform
  pkg_descr0 = do
    (pkg_descr, flags) <-
      case finalizePD
        (configConfigurationsFlags cfg)
        enabled
        satisfies
        compPlatform
        (compilerInfo comp)
        allConstraints
        pkg_descr0 of
        Right r -> return r
        Left missing ->
          dieWithException verbosity $ EncounteredMissingDependency missing

    unless (nullFlagAssignment flags) $
      info verbosity $
        "Flags chosen: "
          ++ intercalate
            ", "
            [ unFlagName fn ++ "=" ++ prettyShow value
            | (fn, value) <- unFlagAssignment flags
            ]

    return (pkg_descr, flags)

-- | Check for use of Cabal features which require compiler support
checkCompilerProblems
  :: Verbosity -> Compiler -> PackageDescription -> ComponentRequestedSpec -> IO ()
checkCompilerProblems verbosity comp pkg_descr enabled = do
  unless
    ( renamingPackageFlagsSupported comp
        || all
          (all (isDefaultIncludeRenaming . mixinIncludeRenaming) . mixins)
          (enabledBuildInfos pkg_descr enabled)
    )
    $ dieWithException verbosity CompilerDoesn'tSupportThinning
  when
    ( any (not . null . reexportedModules) (allLibraries pkg_descr)
        && not (reexportedModulesSupported comp)
    )
    $ dieWithException verbosity CompilerDoesn'tSupportReexports
  when
    ( any (not . null . signatures) (allLibraries pkg_descr)
        && not (backpackSupported comp)
    )
    $ dieWithException verbosity CompilerDoesn'tSupportBackpack

-- | Select dependencies for the package.
configureDependencies
  :: Verbosity
  -> UseExternalInternalDeps
  -> Set LibraryName
  -> Map (PackageName, ComponentName) ComponentId
  -> InstalledPackageIndex
  -- ^ installed packages
  -> Map (PackageName, ComponentName) InstalledPackageInfo
  -- ^ required deps
  -> PackageDescription
  -> ComponentRequestedSpec
  -> IO ([PreExistingComponent], [PromisedComponent])
configureDependencies
  verbosity
  use_external_internal_deps
  packageLibraries
  promisedDeps
  installedPackageSet
  requiredDepsMap
  pkg_descr
  enableSpec = do
    let failedDeps :: [FailedDependency]
        allPkgDeps :: [ResolvedDependency]
        (failedDeps, allPkgDeps) =
          partitionEithers $
            concat
              [ fmap (\s -> (dep, s)) <$> status
              | dep <- enabledBuildDepends pkg_descr enableSpec
              , let status =
                      selectDependency
                        (package pkg_descr)
                        packageLibraries
                        promisedDeps
                        installedPackageSet
                        requiredDepsMap
                        use_external_internal_deps
                        dep
              ]

        internalPkgDeps =
          [ pkgid
          | (_, InternalDependency pkgid) <- allPkgDeps
          ]
        -- NB: we have to SAVE the package name, because this is the only
        -- way we can be able to resolve package names in the package
        -- description.
        externalPkgDeps =
          [ pec
          | (_, ExternalDependency pec) <- allPkgDeps
          ]

        promisedPkgDeps =
          [ fpec
          | (_, PromisedDependency fpec) <- allPkgDeps
          ]

    when
      ( not (null internalPkgDeps)
          && not (newPackageDepsBehaviour pkg_descr)
      )
      $ dieWithException verbosity
      $ LibraryWithinSamePackage internalPkgDeps
    reportFailedDependencies verbosity failedDeps
    reportSelectedDependencies verbosity allPkgDeps

    return (externalPkgDeps, promisedPkgDeps)

-- | Select and apply coverage settings for the build based on the
-- 'ConfigFlags' and 'Compiler'.
configureCoverage
  :: Verbosity
  -> ConfigFlags
  -> Compiler
  -> IO (LBC.BuildOptions -> LBC.BuildOptions)
configureCoverage verbosity cfg comp = do
  let tryExeCoverage = fromFlagOrDefault False (configCoverage cfg)
      tryLibCoverage =
        fromFlagOrDefault
          tryExeCoverage
          (mappend (configCoverage cfg) (configLibCoverage cfg))
  -- TODO: Should we also enforce something here on that --coverage-for cannot
  -- include indefinite components or instantiations?
  if coverageSupported comp
    then do
      let apply buildOptions =
            buildOptions
              { LBC.libCoverage = tryLibCoverage
              , LBC.exeCoverage = tryExeCoverage
              }
      return apply
    else do
      let apply buildOptions =
            buildOptions
              { LBC.libCoverage = False
              , LBC.exeCoverage = False
              }
      when (tryExeCoverage || tryLibCoverage) $
        warn
          verbosity
          ( "The compiler "
              ++ showCompilerId comp
              ++ " does not support "
              ++ "program coverage. Program coverage has been disabled."
          )
      return apply

-- | Compute the effective value of the profiling flags
-- @--enable-library-profiling@ and @--enable-executable-profiling@
-- from the specified 'ConfigFlags'.  This may be useful for
-- external Cabal tools which need to interact with Setup in
-- a backwards-compatible way: the most predictable mechanism
-- for enabling profiling across many legacy versions is to
-- NOT use @--enable-profiling@ and use those two flags instead.
--
-- Note that @--enable-executable-profiling@ also affects profiling
-- of benchmarks and (non-detailed) test suites.
computeEffectiveProfiling :: ConfigFlags -> (Bool {- lib -}, Bool {- exe -})
computeEffectiveProfiling cfg =
  -- The --profiling flag sets the default for both libs and exes,
  -- but can be overridden by --library-profiling, or the old deprecated
  -- --executable-profiling flag.
  --
  -- The --profiling-detail and --library-profiling-detail flags behave
  -- similarly
  let tryExeProfiling =
        fromFlagOrDefault
          False
          (mappend (configProf cfg) (configProfExe cfg))
      tryLibProfiling =
        fromFlagOrDefault
          tryExeProfiling
          (mappend (configProf cfg) (configProfLib cfg))
   in (tryLibProfiling, tryExeProfiling)

-- | Select and apply profiling settings for the build based on the
-- 'ConfigFlags' and 'Compiler'.
configureProfiling
  :: Verbosity
  -> ConfigFlags
  -> Compiler
  -> IO (LBC.BuildOptions -> LBC.BuildOptions)
configureProfiling verbosity cfg comp = do
  let (tryLibProfiling, tryExeProfiling) = computeEffectiveProfiling cfg

      tryExeProfileLevel =
        fromFlagOrDefault
          ProfDetailDefault
          (configProfDetail cfg)
      tryLibProfileLevel =
        fromFlagOrDefault
          ProfDetailDefault
          ( mappend
              (configProfDetail cfg)
              (configProfLibDetail cfg)
          )

      checkProfileLevel (ProfDetailOther other) = do
        warn
          verbosity
          ( "Unknown profiling detail level '"
              ++ other
              ++ "', using default.\nThe profiling detail levels are: "
              ++ intercalate
                ", "
                [name | (name, _, _) <- knownProfDetailLevels]
          )
        return ProfDetailDefault
      checkProfileLevel other = return other

  (exeProfWithoutLibProf, applyProfiling) <-
    if profilingSupported comp
      then do
        exeLevel <- checkProfileLevel tryExeProfileLevel
        libLevel <- checkProfileLevel tryLibProfileLevel
        let apply buildOptions =
              buildOptions
                { LBC.withProfLib = tryLibProfiling
                , LBC.withProfLibDetail = libLevel
                , LBC.withProfExe = tryExeProfiling
                , LBC.withProfExeDetail = exeLevel
                }
        return (tryExeProfiling && not tryLibProfiling, apply)
      else do
        let apply buildOptions =
              buildOptions
                { LBC.withProfLib = False
                , LBC.withProfLibDetail = ProfDetailNone
                , LBC.withProfExe = False
                , LBC.withProfExeDetail = ProfDetailNone
                }
        when (tryExeProfiling || tryLibProfiling) $
          warn
            verbosity
            ( "The compiler "
                ++ showCompilerId comp
                ++ " does not support "
                ++ "profiling. Profiling has been disabled."
            )
        return (False, apply)

  when exeProfWithoutLibProf $
    warn
      verbosity
      ( "Executables will be built with profiling, but library "
          ++ "profiling is disabled. Linking will fail if any executables "
          ++ "depend on the library."
      )

  return applyProfiling

-- -----------------------------------------------------------------------------
-- Configuring package dependencies

reportProgram :: Verbosity -> Program -> Maybe ConfiguredProgram -> IO ()
reportProgram verbosity prog Nothing =
  info verbosity $ "No " ++ programName prog ++ " found"
reportProgram verbosity prog (Just configuredProg) =
  info verbosity $ "Using " ++ programName prog ++ version ++ location
  where
    location = case programLocation configuredProg of
      FoundOnSystem p -> " found on system at: " ++ p
      UserSpecified p -> " given by user at: " ++ p
    version = case programVersion configuredProg of
      Nothing -> ""
      Just v -> " version " ++ prettyShow v

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/package/"

type ResolvedDependency = (Dependency, DependencyResolution)

data DependencyResolution
  = -- | An external dependency from the package database, OR an
    -- internal dependency which we are getting from the package
    -- database.
    ExternalDependency PreExistingComponent
  | -- | A promised dependency, which doesn't yet exist, but should be provided
    -- at the build time.
    --
    -- We have these such that we can configure components without actually
    -- building its dependencies, if these dependencies need to be built later
    -- again. For example, when launching a multi-repl,
    -- we need to build packages in the interactive ghci session, no matter
    -- whether they have been built before.
    -- Building them in the configure phase is then redundant and costs time.
    PromisedDependency PromisedComponent
  | -- | An internal dependency ('PackageId' should be a library name)
    -- which we are going to have to build.  (The
    -- 'PackageId' here is a hack to get a modest amount of
    -- polymorphism out of the Pkg' typeclass.)
    InternalDependency PackageId

-- | Test for a package dependency and record the version we have installed.
selectDependency
  :: PackageId
  -- ^ Package id of current package
  -> Set LibraryName
  -- ^ package libraries
  -> Map (PackageName, ComponentName) ComponentId
  -- ^ Set of components that are promised, i.e. are not installed already. See 'PromisedDependency' for more details.
  -> InstalledPackageIndex
  -- ^ Installed packages
  -> Map (PackageName, ComponentName) InstalledPackageInfo
  -- ^ Packages for which we have been given specific deps to
  -- use
  -> UseExternalInternalDeps
  -- ^ Are we configuring a
  -- single component?
  -> Dependency
  -> [Either FailedDependency DependencyResolution]
selectDependency
  pkgid
  internalIndex
  promisedIndex
  installedIndex
  requiredDepsMap
  use_external_internal_deps
  (Dependency dep_pkgname vr libs) =
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
    if dep_pkgname == pn
      then
        if use_external_internal_deps
          then do_external_internal <$> NES.toList libs
          else do_internal <$> NES.toList libs
      else do_external_external <$> NES.toList libs
    where
      pn = packageName pkgid

      -- It's an internal library, and we're not per-component build
      do_internal lib
        | Set.member lib internalIndex =
            Right $ InternalDependency $ PackageIdentifier dep_pkgname $ packageVersion pkgid
        | otherwise =
            Left $ DependencyMissingInternal dep_pkgname lib

      -- We have to look it up externally
      do_external_external :: LibraryName -> Either FailedDependency DependencyResolution
      do_external_external lib
        | Just cid <- Map.lookup (dep_pkgname, CLibName lib) promisedIndex =
            return $ PromisedDependency (PromisedComponent dep_pkgname (AnnotatedId currentCabalId (CLibName lib) cid))
      do_external_external lib = do
        ipi <- case Map.lookup (dep_pkgname, CLibName lib) requiredDepsMap of
          -- If we know the exact pkg to use, then use it.
          Just pkginstance -> Right pkginstance
          -- Otherwise we just pick an arbitrary instance of the latest version.
          Nothing -> case pickLastIPI $ PackageIndex.lookupInternalDependency installedIndex dep_pkgname vr lib of
            Nothing -> Left (DependencyNotExists dep_pkgname)
            Just pkg -> Right pkg
        return $ ExternalDependency $ ipiToPreExistingComponent ipi

      do_external_internal :: LibraryName -> Either FailedDependency DependencyResolution
      do_external_internal lib
        | Just cid <- Map.lookup (dep_pkgname, CLibName lib) promisedIndex =
            return $ PromisedDependency (PromisedComponent dep_pkgname (AnnotatedId currentCabalId (CLibName lib) cid))
      do_external_internal lib = do
        ipi <- case Map.lookup (dep_pkgname, CLibName lib) requiredDepsMap of
          -- If we know the exact pkg to use, then use it.
          Just pkginstance -> Right pkginstance
          Nothing -> case pickLastIPI $ PackageIndex.lookupInternalDependency installedIndex pn vr lib of
            -- It's an internal library, being looked up externally
            Nothing -> Left (DependencyMissingInternal dep_pkgname lib)
            Just pkg -> Right pkg
        return $ ExternalDependency $ ipiToPreExistingComponent ipi

      pickLastIPI :: [(Version, [InstalledPackageInfo])] -> Maybe InstalledPackageInfo
      pickLastIPI pkgs = safeHead . snd . last =<< nonEmpty pkgs

reportSelectedDependencies
  :: Verbosity
  -> [ResolvedDependency]
  -> IO ()
reportSelectedDependencies verbosity deps =
  info verbosity $
    unlines
      [ "Dependency "
        ++ prettyShow (simplifyDependency dep)
        ++ ": using "
        ++ prettyShow pkgid
      | (dep, resolution) <- deps
      , let pkgid = case resolution of
              ExternalDependency pkg' -> packageId pkg'
              InternalDependency pkgid' -> pkgid'
              PromisedDependency promisedComp -> packageId promisedComp
      ]

reportFailedDependencies :: Verbosity -> [FailedDependency] -> IO ()
reportFailedDependencies _ [] = return ()
reportFailedDependencies verbosity failed =
  dieWithException verbosity $ ReportFailedDependencies failed hackageUrl

-- | List all installed packages in the given package databases.
-- Non-existent package databases do not cause errors, they just get skipped
-- with a warning and treated as empty ones, since technically they do not
-- contain any package.
getInstalledPackages
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDBStack
  -- ^ The stack of package databases.
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity comp mbWorkDir packageDBs progdb = do
  when (null packageDBs) $
    dieWithException verbosity NoPackageDatabaseSpecified

  info verbosity "Reading installed packages..."
  -- do not check empty packagedbs (ghc-pkg would error out)
  packageDBs' <- filterM packageDBExists packageDBs
  case compilerFlavor comp of
    GHC -> GHC.getInstalledPackages verbosity comp mbWorkDir packageDBs' progdb
    GHCJS -> GHCJS.getInstalledPackages verbosity mbWorkDir packageDBs' progdb
    UHC -> UHC.getInstalledPackages verbosity comp packageDBs' progdb
    HaskellSuite{} ->
      HaskellSuite.getInstalledPackages verbosity packageDBs' progdb
    flv ->
      dieWithException verbosity $ HowToFindInstalledPackages flv
  where
    packageDBExists (SpecificPackageDB path0) = do
      let path = interpretSymbolicPath mbWorkDir $ makeSymbolicPath path0
      exists <- doesPathExist path
      unless exists $
        warn verbosity $
          "Package db " <> path <> " does not exist yet"
      return exists
    -- Checking the user and global package dbs is more complicated and needs
    -- way more data. Also ghc-pkg won't error out unless the user/global
    -- pkgdb is overridden with an empty one, so we just don't check for them.
    packageDBExists UserPackageDB = pure True
    packageDBExists GlobalPackageDB = pure True

-- | Like 'getInstalledPackages', but for a single package DB.
--
-- NB: Why isn't this always a fall through to 'getInstalledPackages'?
-- That is because 'getInstalledPackages' performs some sanity checks
-- on the package database stack in question.  However, when sandboxes
-- are involved these sanity checks are not desirable.
getPackageDBContents
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> PackageDB
  -> ProgramDb
  -> IO InstalledPackageIndex
getPackageDBContents verbosity comp mbWorkDir packageDB progdb = do
  info verbosity "Reading installed packages..."
  case compilerFlavor comp of
    GHC -> GHC.getPackageDBContents verbosity mbWorkDir packageDB progdb
    GHCJS -> GHCJS.getPackageDBContents verbosity mbWorkDir packageDB progdb
    -- For other compilers, try to fall back on 'getInstalledPackages'.
    _ -> getInstalledPackages verbosity comp mbWorkDir [packageDB] progdb

-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the installed packages.
getInstalledPackagesMonitorFiles
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD ('Dir Pkg))
  -> PackageDBStack
  -> ProgramDb
  -> Platform
  -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity comp mbWorkDir packageDBs progdb platform =
  case compilerFlavor comp of
    GHC ->
      GHC.getInstalledPackagesMonitorFiles
        verbosity
        mbWorkDir
        platform
        progdb
        packageDBs
    other -> do
      warn verbosity $
        "don't know how to find change monitoring files for "
          ++ "the installed package databases for "
          ++ prettyShow other
      return []

-- | Looks up the 'InstalledPackageInfo' of the given 'UnitId's from the
-- 'PackageDBStack' in the 'LocalBuildInfo'.
getInstalledPackagesById
  :: (Exception (VerboseException exception), Show exception, Typeable exception)
  => Verbosity
  -> LocalBuildInfo
  -> (UnitId -> exception)
  -- ^ Construct an exception that is thrown if a
  -- unit-id is not found in the installed packages,
  -- from the unit-id that is missing.
  -> [UnitId]
  -- ^ The unit ids to lookup in the installed packages
  -> IO [InstalledPackageInfo]
getInstalledPackagesById verbosity lbi@LocalBuildInfo{compiler = comp, withPackageDB = pkgDb, withPrograms = progDb} mkException unitids = do
  let mbWorkDir = mbWorkDirLBI lbi
  ipindex <- getInstalledPackages verbosity comp mbWorkDir pkgDb progDb
  mapM
    ( \uid -> case lookupUnitId ipindex uid of
        Nothing -> dieWithException verbosity (mkException uid)
        Just ipkg -> return ipkg
    )
    unitids

-- | The user interface specifies the package dbs to use with a combination of
-- @--global@, @--user@ and @--package-db=global|user|clear|$file@.
-- This function combines the global/user flag and interprets the package-db
-- flag into a single package db stack.
interpretPackageDbFlags :: Bool -> [Maybe PackageDB] -> PackageDBStack
interpretPackageDbFlags userInstall specificDBs =
  extra initialStack specificDBs
  where
    initialStack
      | userInstall = [GlobalPackageDB, UserPackageDB]
      | otherwise = [GlobalPackageDB]

    extra dbs' [] = dbs'
    extra _ (Nothing : dbs) = extra [] dbs
    extra dbs' (Just db : dbs) = extra (dbs' ++ [db]) dbs

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
combinedConstraints
  :: [PackageVersionConstraint]
  -> [GivenComponent]
  -- ^ installed dependencies
  -> InstalledPackageIndex
  -> Either
      CabalException
      ( [PackageVersionConstraint]
      , Map (PackageName, ComponentName) InstalledPackageInfo
      )
combinedConstraints constraints dependencies installedPackages = do
  when (not (null badComponentIds)) $
    Left $
      CombinedConstraints (dispDependencies badComponentIds)

  -- TODO: we don't check that all dependencies are used!

  return (allConstraints, idConstraintMap)
  where
    allConstraints :: [PackageVersionConstraint]
    allConstraints =
      constraints
        ++ [ thisPackageVersionConstraint (packageId pkg)
           | (_, _, _, Just pkg) <- dependenciesPkgInfo
           ]

    idConstraintMap :: Map (PackageName, ComponentName) InstalledPackageInfo
    idConstraintMap =
      Map.fromList
        -- NB: do NOT use the packageName from
        -- dependenciesPkgInfo!
        [ ((pn, cname), pkg)
        | (pn, cname, _, Just pkg) <- dependenciesPkgInfo
        ]

    -- The dependencies along with the installed package info, if it exists
    dependenciesPkgInfo :: [(PackageName, ComponentName, ComponentId, Maybe InstalledPackageInfo)]
    dependenciesPkgInfo =
      [ (pkgname, CLibName lname, cid, mpkg)
      | GivenComponent pkgname lname cid <- dependencies
      , let mpkg =
              PackageIndex.lookupComponentId
                installedPackages
                cid
      ]

    -- If we looked up a package specified by an installed package id
    -- (i.e. someone has written a hash) and didn't find it then it's
    -- an error.
    badComponentIds =
      [ (pkgname, cname, cid)
      | (pkgname, cname, cid, Nothing) <- dependenciesPkgInfo
      ]

    dispDependencies deps =
      hsep
        [ text "--dependency="
          <<>> quotes
            ( pretty pkgname
                <<>> case cname of
                  CLibName LMainLibName -> ""
                  CLibName (LSubLibName n) -> ":" <<>> pretty n
                  _ -> ":" <<>> pretty cname
                <<>> char '='
                <<>> pretty cid
            )
        | (pkgname, cname, cid) <- deps
        ]

-- -----------------------------------------------------------------------------
-- Configuring program dependencies

configureRequiredPrograms
  :: Verbosity
  -> [LegacyExeDependency]
  -> ProgramDb
  -> IO ProgramDb
configureRequiredPrograms verbosity deps progdb =
  foldM (configureRequiredProgram verbosity) progdb deps

-- | Configure a required program, ensuring that it exists in the PATH
-- (or where the user has specified the program must live) and making it
-- available for use via the 'ProgramDb' interface.  If the program is
-- known (exists in the input 'ProgramDb'), we will make sure that the
-- program matches the required version; otherwise we will accept
-- any version of the program and assume that it is a simpleProgram.
configureRequiredProgram
  :: Verbosity
  -> ProgramDb
  -> LegacyExeDependency
  -> IO ProgramDb
configureRequiredProgram
  verbosity
  progdb
  (LegacyExeDependency progName verRange) =
    case lookupProgramByName progName progdb of
      Just prog ->
        -- If the program has already been configured, use it
        -- (as long as the version is compatible).
        --
        -- Not doing so means falling back to the "simpleProgram" path below,
        -- which might fail if the program has custom logic to find a version
        -- (such as hsc2hs).
        let loc = locationPath $ programLocation prog
         in case programVersion prog of
              Nothing
                | verRange == anyVersion ->
                    return progdb
                | otherwise ->
                    dieWithException verbosity $!
                      UnknownVersionDb (programId prog) verRange loc
              Just version
                | withinRange version verRange ->
                    return progdb
                | otherwise ->
                    dieWithException verbosity $!
                      BadVersionDb (programId prog) version verRange loc
      Nothing ->
        -- Otherwise, try to configure it as a 'simpleProgram' automatically
        case lookupKnownProgram progName progdb of
          Nothing ->
            -- There's a bit of a story behind this line.  In old versions
            -- of Cabal, there were only internal build-tools dependencies.  So the
            -- behavior in this case was:
            --
            --    - If a build-tool dependency was internal, don't do
            --      any checking.
            --
            --    - If it was external, call 'configureRequiredProgram' to
            --      "configure" the executable.  In particular, if
            --      the program was not "known" (present in 'ProgramDb'),
            --      then we would just error.  This was fine, because
            --      the only way a program could be executed from 'ProgramDb'
            --      is if some library code from Cabal actually called it,
            --      and the pre-existing Cabal code only calls known
            --      programs from 'defaultProgramDb', and so if it
            --      is calling something else, you have a Custom setup
            --      script, and in that case you are expected to register
            --      the program you want to call in the ProgramDb.
            --
            -- OK, so that was fine, until I (ezyang, in 2016) refactored
            -- Cabal to support per-component builds.  In this case, what
            -- was previously an internal build-tool dependency now became
            -- an external one, and now previously "internal" dependencies
            -- are now external.  But these are permitted to exist even
            -- when they are not previously configured (something that
            -- can only occur by a Custom script.)
            --
            -- So, I decided, "Fine, let's just accept these in any
            -- case."  Thus this line.  The alternative would have been to
            -- somehow detect when a build-tools dependency was "internal" (by
            -- looking at the unflattened package description) but this
            -- would also be incompatible with future work to support
            -- external executable dependencies: we definitely cannot
            -- assume they will be preinitialized in the 'ProgramDb'.
            configureProgram verbosity (simpleProgram progName) progdb
          Just prog
            -- requireProgramVersion always requires the program have a version
            -- but if the user says "build-depends: foo" ie no version constraint
            -- then we should not fail if we cannot discover the program version.
            | verRange == anyVersion -> do
                (_, progdb') <- requireProgram verbosity prog progdb
                return progdb'
            | otherwise -> do
                (_, _, progdb') <- requireProgramVersion verbosity prog verRange progdb
                return progdb'

-- -----------------------------------------------------------------------------
-- Configuring pkg-config package dependencies

configurePkgconfigPackages
  :: Verbosity
  -> PackageDescription
  -> ProgramDb
  -> ComponentRequestedSpec
  -> IO (PackageDescription, ProgramDb)
configurePkgconfigPackages verbosity pkg_descr progdb enabled
  | null allpkgs = return (pkg_descr, progdb)
  | otherwise = do
      (_, _, progdb') <-
        requireProgramVersion
          (lessVerbose verbosity)
          pkgConfigProgram
          (orLaterVersion $ mkVersion [0, 9, 0])
          progdb
      traverse_ requirePkg allpkgs
      mlib' <- traverse addPkgConfigBILib (library pkg_descr)
      libs' <- traverse addPkgConfigBILib (subLibraries pkg_descr)
      exes' <- traverse addPkgConfigBIExe (executables pkg_descr)
      tests' <- traverse addPkgConfigBITest (testSuites pkg_descr)
      benches' <- traverse addPkgConfigBIBench (benchmarks pkg_descr)
      let pkg_descr' =
            pkg_descr
              { library = mlib'
              , subLibraries = libs'
              , executables = exes'
              , testSuites = tests'
              , benchmarks = benches'
              }
      return (pkg_descr', progdb')
  where
    allpkgs = concatMap pkgconfigDepends (enabledBuildInfos pkg_descr enabled)
    pkgconfig =
      getDbProgramOutput
        (lessVerbose verbosity)
        pkgConfigProgram
        progdb

    requirePkg dep@(PkgconfigDependency pkgn range) = do
      version <-
        pkgconfig ["--modversion", pkg]
          `catchIO` (\_ -> dieWithException verbosity $ PkgConfigNotFound pkg versionRequirement)
          `catchExit` (\_ -> dieWithException verbosity $ PkgConfigNotFound pkg versionRequirement)
      let trim = dropWhile isSpace . dropWhileEnd isSpace
      let v = PkgconfigVersion (toUTF8BS $ trim version)
      if not (withinPkgconfigVersionRange v range)
        then dieWithException verbosity $ BadVersion pkg versionRequirement v
        else info verbosity (depSatisfied v)
      where
        depSatisfied v =
          "Dependency "
            ++ prettyShow dep
            ++ ": using version "
            ++ prettyShow v

        versionRequirement
          | isAnyPkgconfigVersion range = ""
          | otherwise = " version " ++ prettyShow range

        pkg = unPkgconfigName pkgn

    -- Adds pkgconfig dependencies to the build info for a component
    addPkgConfigBI compBI setCompBI comp = do
      bi <- pkgconfigBuildInfo (pkgconfigDepends (compBI comp))
      return $ setCompBI comp (compBI comp `mappend` bi)

    -- Adds pkgconfig dependencies to the build info for a library
    addPkgConfigBILib = addPkgConfigBI libBuildInfo $
      \lib bi -> lib{libBuildInfo = bi}

    -- Adds pkgconfig dependencies to the build info for an executable
    addPkgConfigBIExe = addPkgConfigBI buildInfo $
      \exe bi -> exe{buildInfo = bi}

    -- Adds pkgconfig dependencies to the build info for a test suite
    addPkgConfigBITest = addPkgConfigBI testBuildInfo $
      \test bi -> test{testBuildInfo = bi}

    -- Adds pkgconfig dependencies to the build info for a benchmark
    addPkgConfigBIBench = addPkgConfigBI benchmarkBuildInfo $
      \bench bi -> bench{benchmarkBuildInfo = bi}

    pkgconfigBuildInfo :: [PkgconfigDependency] -> IO BuildInfo
    pkgconfigBuildInfo [] = return mempty
    pkgconfigBuildInfo pkgdeps = do
      let pkgs = nub [prettyShow pkg | PkgconfigDependency pkg _ <- pkgdeps]
      ccflags <- pkgconfig ("--cflags" : pkgs)
      ldflags <- pkgconfig ("--libs" : pkgs)
      ldflags_static <- pkgconfig ("--libs" : "--static" : pkgs)
      return (ccLdOptionsBuildInfo (words ccflags) (words ldflags) (words ldflags_static))

-- | Makes a 'BuildInfo' from C compiler and linker flags.
--
-- This can be used with the output from configuration programs like pkg-config
-- and similar package-specific programs like mysql-config, freealut-config etc.
-- For example:
--
-- > ccflags <- getDbProgramOutput verbosity prog progdb ["--cflags"]
-- > ldflags <- getDbProgramOutput verbosity prog progdb ["--libs"]
-- > ldflags_static <- getDbProgramOutput verbosity prog progdb ["--libs", "--static"]
-- > return (ccldOptionsBuildInfo (words ccflags) (words ldflags) (words ldflags_static))
ccLdOptionsBuildInfo :: [String] -> [String] -> [String] -> BuildInfo
ccLdOptionsBuildInfo cflags ldflags ldflags_static =
  let (includeDirs', cflags') = partition ("-I" `isPrefixOf`) cflags
      (extraLibs', ldflags') = partition ("-l" `isPrefixOf`) ldflags
      (extraLibDirs', ldflags'') = partition ("-L" `isPrefixOf`) ldflags'
      (extraLibsStatic') = filter ("-l" `isPrefixOf`) ldflags_static
      (extraLibDirsStatic') = filter ("-L" `isPrefixOf`) ldflags_static
   in mempty
        { includeDirs = map (makeSymbolicPath . drop 2) includeDirs'
        , extraLibs = map (drop 2) extraLibs'
        , extraLibDirs = map (makeSymbolicPath . drop 2) extraLibDirs'
        , extraLibsStatic = map (drop 2) extraLibsStatic'
        , extraLibDirsStatic = map (makeSymbolicPath . drop 2) extraLibDirsStatic'
        , ccOptions = cflags'
        , ldOptions = ldflags''
        }

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAuxEx
  :: ConfigFlags
  -> IO (Compiler, Platform, ProgramDb)
configCompilerAuxEx cfg = do
  programDb <- mkProgramDb cfg defaultProgramDb
  let common = configCommonFlags cfg
      verbosity = fromFlag $ setupVerbosity common
  configCompilerEx
    (flagToMaybe $ configHcFlavor cfg)
    (flagToMaybe $ configHcPath cfg)
    (flagToMaybe $ configHcPkg cfg)
    programDb
    verbosity

configCompilerEx
  :: Maybe CompilerFlavor
  -> Maybe FilePath
  -> Maybe FilePath
  -> ProgramDb
  -> Verbosity
  -> IO (Compiler, Platform, ProgramDb)
configCompilerEx Nothing _ _ _ verbosity = dieWithException verbosity UnknownCompilerException
configCompilerEx (Just hcFlavor) hcPath hcPkg progdb verbosity = do
  (comp, maybePlatform, programDb) <- case hcFlavor of
    GHC -> GHC.configure verbosity hcPath hcPkg progdb
    GHCJS -> GHCJS.configure verbosity hcPath hcPkg progdb
    UHC -> UHC.configure verbosity hcPath hcPkg progdb
    HaskellSuite{} -> HaskellSuite.configure verbosity hcPath hcPkg progdb
    _ -> dieWithException verbosity UnknownCompilerException
  return (comp, fromMaybe buildPlatform maybePlatform, programDb)

-- -----------------------------------------------------------------------------
-- Testing C lib and header dependencies

-- Try to build a test C program which includes every header and links every
-- lib. If that fails, try to narrow it down by preprocessing (only) and linking
-- with individual headers and libs.  If none is the obvious culprit then give a
-- generic error message.
-- TODO: produce a log file from the compiler errors, if any.
checkForeignDeps :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
checkForeignDeps pkg lbi verbosity =
  ifBuildsWith
    allHeaders
    (commonCcArgs ++ makeLdArgs allLibs) -- I'm feeling lucky
    (return ())
    ( do
        missingLibs <- findMissingLibs
        missingHdr <- findOffendingHdr
        explainErrors missingHdr missingLibs
    )
  where
    allHeaders = collectField (fmap getSymbolicPath . includes)
    allLibs =
      collectField $
        if withFullyStaticExe lbi
          then extraLibsStatic
          else extraLibs

    ifBuildsWith headers args success failure = do
      checkDuplicateHeaders
      ok <- builds (makeProgram headers) args
      if ok then success else failure

    -- Ensure that there is only one header with a given name
    -- in either the generated (most likely by `configure`)
    -- build directory (e.g. `dist/build`) or in the source directory.
    --
    -- If it exists in both, we'll remove the one in the source
    -- directory, as the generated should take precedence.
    --
    -- C compilers like to prefer source local relative includes,
    -- so the search paths provided to the compiler via -I are
    -- ignored if the included file can be found relative to the
    -- including file.  As such we need to take drastic measures
    -- and delete the offending file in the source directory.
    checkDuplicateHeaders = do
      let relIncDirs = filter (not . isAbsolute) (collectField (fmap getSymbolicPath . includeDirs))
          isHeader = isSuffixOf ".h"
      genHeaders <- for relIncDirs $ \dir ->
        fmap (dir </>) . filter isHeader
          <$> listDirectory (i (buildDir lbi) </> dir) `catchIO` (\_ -> return [])
      srcHeaders <- for relIncDirs $ \dir ->
        fmap (dir </>) . filter isHeader
          <$> listDirectory (baseDir </> dir) `catchIO` (\_ -> return [])
      let commonHeaders = concat genHeaders `intersect` concat srcHeaders
      for_ commonHeaders $ \hdr -> do
        warn verbosity $
          "Duplicate header found in "
            ++ (getSymbolicPath (buildDir lbi) </> hdr)
            ++ " and "
            ++ (baseDir </> hdr)
            ++ "; removing "
            ++ (baseDir </> hdr)
        removeFile (baseDir </> hdr)

    findOffendingHdr =
      ifBuildsWith
        allHeaders
        ccArgs
        (return Nothing)
        (go . tail . NEL.inits $ allHeaders)
      where
        go [] = return Nothing -- cannot happen
        go (hdrs : hdrsInits) =
          -- Try just preprocessing first
          ifBuildsWith
            hdrs
            cppArgs
            -- If that works, try compiling too
            ( ifBuildsWith
                hdrs
                ccArgs
                (go hdrsInits)
                (return . fmap Right . safeLast $ hdrs)
            )
            (return . fmap Left . safeLast $ hdrs)

        cppArgs = "-E" : commonCppArgs -- preprocess only
        ccArgs = "-c" : commonCcArgs -- don't try to link
    findMissingLibs =
      ifBuildsWith
        []
        (makeLdArgs allLibs)
        (return [])
        (filterM (fmap not . libExists) allLibs)

    libExists lib = builds (makeProgram []) (makeLdArgs [lib])

    common = configCommonFlags $ configFlags lbi
    baseDir = packageRoot common

    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i = interpretSymbolicPathLBI lbi
    mbWorkDir = mbWorkDirLBI lbi

    commonCppArgs =
      platformDefines lbi
        -- TODO: This is a massive hack, to work around the
        -- fact that the test performed here should be
        -- PER-component (c.f. the "I'm Feeling Lucky"; we
        -- should NOT be glomming everything together.)
        ++ ["-I" ++ i (buildDir lbi </> makeRelativePathEx "autogen")]
        -- `configure' may generate headers in the build directory
        ++ [ "-I" ++ i (buildDir lbi </> unsafeCoerceSymbolicPath dir)
           | dir <- mapMaybe symbolicPathRelative_maybe $ ordNub (collectField includeDirs)
           ]
        -- we might also reference headers from the
        -- packages directory.
        ++ [ "-I" ++ baseDir </> getSymbolicPath dir
           | dir <- mapMaybe symbolicPathRelative_maybe $ ordNub (collectField includeDirs)
           ]
        ++ [ "-I" ++ dir
           | dir <- ordNub (collectField (fmap getSymbolicPath . includeDirs))
           , isAbsolute dir
           ]
        ++ ["-I" ++ baseDir]
        ++ collectField cppOptions
        ++ collectField ccOptions
        ++ [ "-I" ++ dir
           | dir <-
              ordNub
                [ dir
                | dep <- deps
                , dir <- IPI.includeDirs dep
                ]
                -- dedupe include dirs of dependencies
                -- to prevent quadratic blow-up
           ]
        ++ [ opt
           | dep <- deps
           , opt <- IPI.ccOptions dep
           ]

    commonCcArgs =
      commonCppArgs
        ++ collectField ccOptions
        ++ [ opt
           | dep <- deps
           , opt <- IPI.ccOptions dep
           ]

    commonLdArgs =
      [ "-L" ++ getSymbolicPath dir
      | dir <-
          ordNub $
            collectField
              ( if withFullyStaticExe lbi
                  then extraLibDirsStatic
                  else extraLibDirs
              )
      ]
        ++ collectField ldOptions
        ++ [ "-L" ++ dir
           | dir <-
              ordNub
                [ dir
                | dep <- deps
                , dir <-
                    if withFullyStaticExe lbi
                      then IPI.libraryDirsStatic dep
                      else IPI.libraryDirs dep
                ]
           ]
    -- TODO: do we also need dependent packages' ld options?
    makeLdArgs libs = ["-l" ++ lib | lib <- libs] ++ commonLdArgs

    makeProgram hdrs =
      unlines $
        ["#include \"" ++ hdr ++ "\"" | hdr <- hdrs]
          ++ ["int main(int argc, char** argv) { return 0; }"]

    collectField f = concatMap f allBi
    allBi = enabledBuildInfos pkg (componentEnabledSpec lbi)
    deps = PackageIndex.topologicalOrder (installedPkgs lbi)

    builds :: String -> [ProgArg] -> IO Bool
    builds program args =
      do
        tempDir <- makeSymbolicPath <$> getTemporaryDirectory
        withTempFileCwd mbWorkDir tempDir ".c" $ \cName cHnd ->
          withTempFileCwd mbWorkDir tempDir "" $ \oNname oHnd -> do
            hPutStrLn cHnd program
            hClose cHnd
            hClose oHnd
            _ <-
              getDbProgramOutputCwd
                verbosity
                mbWorkDir
                gccProgram
                (withPrograms lbi)
                (getSymbolicPath cName : "-o" : getSymbolicPath oNname : args)
            return True
        `catchIO` (\_ -> return False)
        `catchExit` (\_ -> return False)

    explainErrors Nothing [] = return () -- should be impossible!
    explainErrors _ _
      | isNothing . lookupProgram gccProgram . withPrograms $ lbi =
          dieWithException verbosity NoWorkingGcc
    explainErrors hdr libs =
      dieWithException verbosity $ ExplainErrors hdr libs

-- | Output package check warnings and errors. Exit if any errors.
checkPackageProblems
  :: Verbosity
  -> FilePath
  -- ^ Path to the @.cabal@ file's directory
  -> GenericPackageDescription
  -> PackageDescription
  -> IO ()
checkPackageProblems verbosity dir gpkg pkg = do
  ioChecks <- checkPackageFiles verbosity pkg dir
  let pureChecks = checkPackage gpkg
      (errors, warnings) =
        partitionEithers (M.mapMaybe classEW $ pureChecks ++ ioChecks)
  if null errors
    then traverse_ (warn verbosity) (map ppPackageCheck warnings)
    else dieWithException verbosity $ CheckPackageProblems (map ppPackageCheck errors)
  where
    -- Classify error/warnings. Left: error, Right: warning.
    classEW :: PackageCheck -> Maybe (Either PackageCheck PackageCheck)
    classEW e@(PackageBuildImpossible _) = Just (Left e)
    classEW w@(PackageBuildWarning _) = Just (Right w)
    classEW (PackageDistSuspicious _) = Nothing
    classEW (PackageDistSuspiciousWarn _) = Nothing
    classEW (PackageDistInexcusable _) = Nothing

-- | Preform checks if a relocatable build is allowed
checkRelocatable
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
checkRelocatable verbosity pkg lbi =
  sequence_
    [ checkOS
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
    checkOS =
      unless (os `elem` [OSX, Linux]) $
        dieWithException verbosity $
          NoOSSupport os
      where
        (Platform _ os) = hostPlatform lbi

    -- Check if the Compiler support relocatable builds
    checkCompiler =
      unless (compilerFlavor comp `elem` [GHC]) $
        dieWithException verbosity $
          NoCompilerSupport (show comp)
      where
        comp = compiler lbi

    -- Check if all the install dirs are relative to same prefix
    packagePrefixRelative =
      unless (relativeInstallDirs installDirs) $
        dieWithException verbosity $
          InstallDirsNotPrefixRelative (installDirs)
      where
        -- NB: should be good enough to check this against the default
        -- component ID, but if we wanted to be strictly correct we'd
        -- check for each ComponentId.
        installDirs = absoluteInstallDirs pkg lbi NoCopyDest
        p = prefix installDirs
        relativeInstallDirs (InstallDirs{..}) =
          all
            isJust
            ( fmap
                (stripPrefix p)
                [ bindir
                , libdir
                , dynlibdir
                , libexecdir
                , includedir
                , datadir
                , docdir
                , mandir
                , htmldir
                , haddockdir
                , sysconfdir
                ]
            )

    -- Check if the library dirs of the dependencies that are in the package
    -- database to which the package is installed are relative to the
    -- prefix of the package
    depsPrefixRelative = do
      pkgr <- GHC.pkgRoot verbosity lbi (registrationPackageDB (withPackageDB lbi))
      traverse_ (doCheck $ getSymbolicPath pkgr) ipkgs
      where
        doCheck pkgr ipkg
          | maybe False (== pkgr) (IPI.pkgRoot ipkg) =
              for_ (IPI.libraryDirs ipkg) $ \libdir -> do
                -- When @prefix@ is not under @pkgroot@,
                -- @shortRelativePath prefix pkgroot@ will return a path with
                -- @..@s and following check will fail without @canonicalizePath@.
                canonicalized <- canonicalizePath libdir
                -- The @prefix@ itself must also be canonicalized because
                -- canonicalizing @libdir@ may expand symlinks which would make
                -- @prefix@ no longer being a prefix of @canonical libdir@,
                -- while @canonical p@ could be a prefix of @canonical libdir@
                p' <- canonicalizePath p
                unless (p' `isPrefixOf` canonicalized) $
                  dieWithException verbosity $
                    LibDirDepsPrefixNotRelative libdir p
          | otherwise =
              return ()
        -- NB: should be good enough to check this against the default
        -- component ID, but if we wanted to be strictly correct we'd
        -- check for each ComponentId.
        installDirs = absoluteInstallDirs pkg lbi NoCopyDest
        p = prefix installDirs
        ipkgs = PackageIndex.allPackages (installedPkgs lbi)

-- -----------------------------------------------------------------------------
-- Testing foreign library requirements

unsupportedForeignLibs :: Compiler -> Platform -> [ForeignLib] -> [String]
unsupportedForeignLibs comp platform =
  mapMaybe (checkForeignLibSupported comp platform)

checkForeignLibSupported :: Compiler -> Platform -> ForeignLib -> Maybe String
checkForeignLibSupported comp platform flib = go (compilerFlavor comp)
  where
    go :: CompilerFlavor -> Maybe String
    go GHC
      | compilerVersion comp < mkVersion [7, 8] =
          unsupported
            [ "Building foreign libraries is only supported with GHC >= 7.8"
            ]
      | otherwise = goGhcPlatform platform
    go _ =
      unsupported
        [ "Building foreign libraries is currently only supported with ghc"
        ]

    goGhcPlatform :: Platform -> Maybe String
    goGhcPlatform (Platform _ OSX) = goGhcOsx (foreignLibType flib)
    goGhcPlatform (Platform _ Linux) = goGhcLinux (foreignLibType flib)
    goGhcPlatform (Platform I386 Windows) = goGhcWindows (foreignLibType flib)
    goGhcPlatform (Platform X86_64 Windows) = goGhcWindows (foreignLibType flib)
    goGhcPlatform _ =
      unsupported
        [ "Building foreign libraries is currently only supported on Mac OS, "
        , "Linux and Windows"
        ]

    goGhcOsx :: ForeignLibType -> Maybe String
    goGhcOsx ForeignLibNativeShared
      | not (null (foreignLibModDefFile flib)) =
          unsupported
            [ "Module definition file not supported on OSX"
            ]
      | not (null (foreignLibVersionInfo flib)) =
          unsupported
            [ "Foreign library versioning not currently supported on OSX"
            ]
      | otherwise =
          Nothing
    goGhcOsx _ =
      unsupported
        [ "We can currently only build shared foreign libraries on OSX"
        ]

    goGhcLinux :: ForeignLibType -> Maybe String
    goGhcLinux ForeignLibNativeShared
      | not (null (foreignLibModDefFile flib)) =
          unsupported
            [ "Module definition file not supported on Linux"
            ]
      | not (null (foreignLibVersionInfo flib))
          && not (null (foreignLibVersionLinux flib)) =
          unsupported
            [ "You must not specify both lib-version-info and lib-version-linux"
            ]
      | otherwise =
          Nothing
    goGhcLinux _ =
      unsupported
        [ "We can currently only build shared foreign libraries on Linux"
        ]

    goGhcWindows :: ForeignLibType -> Maybe String
    goGhcWindows ForeignLibNativeShared
      | not standalone =
          unsupported
            [ "We can currently only build standalone libraries on Windows. Use\n"
            , "  if os(Windows)\n"
            , "    options: standalone\n"
            , "in your foreign-library stanza."
            ]
      | not (null (foreignLibVersionInfo flib)) =
          unsupported
            [ "Foreign library versioning not currently supported on Windows.\n"
            , "You can specify module definition files in the mod-def-file field."
            ]
      | otherwise =
          Nothing
    goGhcWindows _ =
      unsupported
        [ "We can currently only build shared foreign libraries on Windows"
        ]

    standalone :: Bool
    standalone = ForeignLibStandalone `elem` foreignLibOptions flib

    unsupported :: [String] -> Maybe String
    unsupported = Just . concat
