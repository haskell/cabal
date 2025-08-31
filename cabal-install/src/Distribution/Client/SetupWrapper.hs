{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{- FOURMOLU_DISABLE -}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.SetupWrapper
-- Copyright   :  (c) The University of Glasgow 2006,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- An interface to building and installing Cabal packages.
-- If the @Built-Type@ field is specified as something other than
-- 'Custom', and the current version of Cabal is acceptable, this performs
-- setup actions directly.  Otherwise it builds the setup script and
-- runs it with the given arguments.
module Distribution.Client.SetupWrapper
  ( getSetup
  , runSetup
  , runSetupCommand
  , SetupRunnerArgs(..)
  , SPostConfigurePhase(..)
  , InLibraryArgs(..)
  , SetupRunnerRes
  , InLibraryLBI(..)
  , RightFlagsForPhase
  , setupWrapper
  , SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , externalSetupMethod
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Backpack as Backpack
import Distribution.CabalSpecVersion (cabalSpecMinimumLibraryVersion)
import Distribution.Package
  ( ComponentId
  , PackageId
  , PackageIdentifier (..)
  , mkPackageName
  , newSimpleUnitId
  , packageName
  , packageVersion
  , unsafeMkDefUnitId
  )
import Distribution.PackageDescription
  ( BuildType (..)
  , GenericPackageDescription (packageDescription)
  , PackageDescription (..)
  , buildType
  , specVersion
  )
import qualified Distribution.Simple as Simple
import Distribution.Simple.Build.Macros
  ( generatePackageVersionMacros
  )
import Distribution.Simple.BuildPaths
  ( exeExtension
  )
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
  hiding ( getInstalledPackages )
import Distribution.Simple.PackageDescription
  ( readGenericPackageDescription
  )
import Distribution.Simple.PreProcess
  ( ppUnlit
  , runSimplePreProcessor
  )
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Find
  ( programSearchPathAsPATHVar
  )
import Distribution.Simple.Program.Run
  ( getEffectiveEnvironment
  )
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Types.ModuleRenaming (defaultRenaming)
import Distribution.Version
  ( Version
  , VersionRange
  , anyVersion
  , intersectVersionRanges
  , mkVersion
  , orLaterVersion
  , versionNumbers
  , withinRange
  )

import Distribution.Client.Config
  ( defaultCacheDir
  )
import Distribution.Client.FileMonitor
  ( MonitorFilePath )
import Distribution.Client.IndexUtils
  ( getInstalledPackages
  )
import Distribution.Client.JobControl
  ( Lock
  , criticalSection
  )
import Distribution.Client.Types
import Distribution.Client.Utils
  ( existsAndIsMoreRecentThan
  , makeRelativeToDirS
#ifdef mingw32_HOST_OS
  , canonicalizePathNoThrow
#endif
  , moreRecentFile
  , tryCanonicalizePath
  )
import Distribution.Utils.Path
  hiding ( (</>), (<.>) )
import qualified Distribution.Utils.Path as Cabal.Path
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Command
  ( CommandUI (..)
  , commandShowOptions
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program.GHC
  ( GhcMode (..)
  , GhcOptions (..)
  , renderGhcOptions
  )
import Distribution.Simple.Utils
  ( cabalVersion
  , copyFileVerbose
  , createDirectoryIfMissingVerbose
  , debug
  , die'
  , dieWithException
  , info
  , infoNoWrap
  , installExecutableFile
  , maybeExit
  , rawSystemProc
  , rewriteFileEx
  , rewriteFileLBS
  , tryFindPackageDesc
  )
import Distribution.Utils.Generic
  ( safeHead
  )

import Distribution.Compat.Stack
import Distribution.ReadE
import Distribution.Simple.Setup
import Distribution.Client.Compat.ExecutablePath (getExecutablePath)
import Distribution.Compat.Process (proc)
import Distribution.System (Platform (..), buildPlatform)
import Distribution.Utils.NubList
  ( toNubListR
  )
import Distribution.Types.LocalBuildInfo ( LocalBuildInfo )
import qualified Distribution.Types.LocalBuildInfo as LBI
import Distribution.Verbosity
import Distribution.Client.Errors
import qualified Distribution.Client.InLibrary as InLibrary
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.SetupHooks.Version
  ( hooksVersion )
import Distribution.Client.SetupHooks.CallHooksExe
  ( externalSetupHooksABI, hooksProgFilePath )
import Data.List (foldl1')
import Data.Kind (Type, Constraint)
import qualified Data.Map.Lazy as Map
import Data.Type.Equality  ( type (==) )
import Data.Type.Bool      ( If )
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import System.IO (Handle, hPutStr)
import System.Process (StdStream (..))
import qualified System.Process as Process

import qualified Data.ByteString.Lazy as BS

#ifdef mingw32_HOST_OS
import Distribution.Simple.Utils
         ( withTempDirectory )

import Control.Exception   ( bracket )
import System.Directory    ( doesDirectoryExist )
import System.FilePath     ( equalFilePath, takeDirectory, takeFileName )
import qualified System.Win32 as Win32
#endif

data AllowInLibrary
  = AllowInLibrary
  | Don'tAllowInLibrary
  deriving Eq

data SetupKind
  = InLibrary
  | GeneralSetup

-- | If we end up using the in-library method, we use the v'InLibraryLBI'
-- constructor. If not, we use the 'NotInLibraryNoLBI' constructor.
--
-- NB: we don't know ahead of time whether we can use the in-library method;
-- e.g. for a package with Hooks build-type, it depends on whether the Cabal
-- version used by the package matches with the Cabal version that cabal-install
-- was built against.
data InLibraryLBI
  = InLibraryLBI LocalBuildInfo
  | NotInLibraryNoLBI

data SPostConfigurePhase (flags :: Type) where
  SBuildPhase    :: SPostConfigurePhase BuildFlags
  SHaddockPhase  :: SPostConfigurePhase HaddockFlags
  SReplPhase     :: SPostConfigurePhase ReplFlags
  SCopyPhase     :: SPostConfigurePhase CopyFlags
  SRegisterPhase :: SPostConfigurePhase RegisterFlags
  STestPhase     :: SPostConfigurePhase TestFlags
  SBenchPhase    :: SPostConfigurePhase BenchmarkFlags

data SetupWrapperSpec
  = TryInLibrary Type
  | UseGeneralSetup

type family RightFlagsForPhase (flags :: Type) (setupSpec :: SetupWrapperSpec) :: Constraint where
  RightFlagsForPhase flags UseGeneralSetup = ()
  RightFlagsForPhase flags (TryInLibrary flags') = flags ~ flags'

data SetupRunnerArgs (spec :: SetupWrapperSpec) where
  NotInLibrary
    :: SetupRunnerArgs UseGeneralSetup
  InLibraryArgs
    :: InLibraryArgs flags
    -> SetupRunnerArgs (TryInLibrary flags)

data InLibraryArgs (flags :: Type) where
  InLibraryConfigureArgs
    :: ElaboratedSharedConfig
    -> ElaboratedReadyPackage
    -> InLibraryArgs ConfigFlags
  InLibraryPostConfigureArgs
    :: SPostConfigurePhase flags
    -> InLibraryLBI
    -> InLibraryArgs flags

type family SetupRunnerRes (spec :: SetupWrapperSpec) where
  SetupRunnerRes UseGeneralSetup = ()
  SetupRunnerRes (TryInLibrary phase) = InLibraryPhaseRes phase

type family InLibraryPhaseRes flags where
  InLibraryPhaseRes ConfigFlags  = InLibraryLBI
  InLibraryPhaseRes BuildFlags   = [MonitorFilePath]
  InLibraryPhaseRes HaddockFlags = [MonitorFilePath]
  InLibraryPhaseRes ReplFlags    = ()
  InLibraryPhaseRes _            = ()

-- | @Setup@ encapsulates the outcome of configuring a setup method to build a
-- particular package.
data Setup kind = Setup
  { setupMethod :: SetupMethod kind
  , setupScriptOptions :: SetupScriptOptions
  , setupVersion :: Version
  , setupBuildType :: BuildType
  , setupPackage :: PackageDescription
  }

data ASetup = forall kind. ASetup ( Setup kind )

-- | @SetupMethod@ represents one of the methods used to run Cabal commands.
data SetupMethod (kind :: SetupKind) where
  -- | Directly use Cabal library functions, bypassing the Setup
  -- mechanism entirely.
    LibraryMethod :: SetupMethod InLibrary
  -- | run Cabal commands through @cabal@ as a child process,
  -- using @cabal --act-as-setup@
    SelfExecMethod :: SetupMethod GeneralSetup
  -- | run Cabal commands through a custom \"Setup\" executable
    ExternalMethod :: FilePath -> SetupMethod GeneralSetup

-- TODO: The 'setupWrapper' and 'SetupScriptOptions' should be split into two
-- parts: one that has no policy and just does as it's told with all the
-- explicit options, and an optional initial part that applies certain
-- policies (like if we should add the Cabal lib as a dep, and if so which
-- version). This could be structured as an action that returns a fully
-- elaborated 'SetupScriptOptions' containing no remaining policy choices.
--
-- See also the discussion at https://github.com/haskell/cabal/pull/3094

-- | @SetupScriptOptions@ are options used to configure and run 'Setup', as
-- opposed to options given to the Cabal command at runtime.
data SetupScriptOptions = SetupScriptOptions
  { useCabalVersion :: VersionRange
  -- ^ The version of the Cabal library to use (if 'useDependenciesExclusive'
  -- is not set). A suitable version of the Cabal library must be installed
  -- (or for some build-types be the one cabal-install was built with).
  --
  -- The version found also determines the version of the Cabal specification
  -- that we us for talking to the Setup.hs, unless overridden by
  -- 'useCabalSpecVersion'.
  , useCabalSpecVersion :: Maybe Version
  -- ^ This is the version of the Cabal specification that we believe that
  -- this package uses. This affects the semantics and in particular the
  -- Setup command line interface.
  --
  -- This is similar to 'useCabalVersion' but instead of probing the system
  -- for a version of the /Cabal library/ you just say exactly which version
  -- of the /spec/ we will use. Using this also avoid adding the Cabal
  -- library as an additional dependency, so add it to 'useDependencies'
  -- if needed.
  , useCompiler :: Maybe Compiler
  , usePlatform :: Maybe Platform
  , usePackageDB :: PackageDBStackCWD
  , usePackageIndex :: Maybe InstalledPackageIndex
  , useProgramDb :: ProgramDb
  , useDistPref :: SymbolicPath Pkg (Dir Dist)
  , useLoggingHandle :: Maybe Handle
  , useWorkingDir :: Maybe (SymbolicPath CWD (Dir Pkg))
  , useExtraPathEnv :: [FilePath]
  -- ^ Extra things to add to PATH when invoking the setup script.
  , useExtraEnvOverrides :: [(String, Maybe FilePath)]
  -- ^ Extra environment variables paired with overrides, where
  --
  -- * @'Just' v@ means \"set the environment variable's value to @v@\".
  -- * 'Nothing' means \"unset the environment variable\".
  , useDependencies :: [(ComponentId, PackageId)]
  -- ^ List of dependencies to use when building Setup.hs.
  , useDependenciesExclusive :: Bool
  -- ^ Is the list of setup dependencies exclusive?
  --
  -- When this is @False@, if we compile the Setup.hs script we do so with the
  -- list in 'useDependencies' but all other packages in the environment are
  -- also visible. A suitable version of @Cabal@ library (see
  -- 'useCabalVersion') is also added to the list of dependencies, unless
  -- 'useDependencies' already contains a Cabal dependency.
  --
  -- When @True@, only the 'useDependencies' packages are used, with other
  -- packages in the environment hidden.
  --
  -- This feature is here to support the setup stanza in .cabal files that
  -- specifies explicit (and exclusive) dependencies, as well as the old
  -- style with no dependencies.
  , useVersionMacros :: Bool
  -- ^ Should we build the Setup.hs with CPP version macros available?
  -- We turn this on when we have a setup stanza in .cabal that declares
  -- explicit setup dependencies.
  , -- Used only by 'cabal clean' on Windows.
    --
    -- Note: win32 clean hack
    -------------------------
    -- On Windows, running './dist/setup/setup clean' doesn't work because the
    -- setup script will try to delete itself (which causes it to fail horribly,
    -- unlike on Linux). So we have to move the setup exe out of the way first
    -- and then delete it manually. This applies only to the external setup
    -- method.
    useWin32CleanHack :: Bool
  , -- Used only when calling setupWrapper from parallel code to serialise
    -- access to the setup cache; should be Nothing otherwise.
    --
    -- Note: setup exe cache
    ------------------------
    -- When we are installing in parallel, we always use the external setup
    -- method. Since compiling the setup script each time adds noticeable
    -- overhead, we use a shared setup script cache
    -- ('$XDG_CACHE_HOME/cabal/setup-exe-cache'). For each (compiler, platform, Cabal
    -- version) combination the cache holds a compiled setup script
    -- executable. This only affects the Simple build type; for the Custom,
    -- Configure and Make build types we always compile the setup script anew.
    setupCacheLock :: Maybe Lock
  , isInteractive :: Bool
  -- ^ Is the task we are going to run an interactive foreground task,
  -- or an non-interactive background task? Based on this flag we
  -- decide whether or not to delegate ctrl+c to the spawned task
  , isMainLibOrExeComponent :: Bool
  -- ^ Let the setup script logic know if it is being run to build a main
  -- library or executable component.  This is used to determine if we should
  -- use the configure command, if the build-type is 'Configure'.  For
  -- configure, only the main library and execomponents have 'configure'
  -- support, and thus we can skip running configure for other components.
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions =
  SetupScriptOptions
    { useCabalVersion = anyVersion
    , useCabalSpecVersion = Nothing
    , useCompiler = Nothing
    , usePlatform = Nothing
    , usePackageDB = [GlobalPackageDB, UserPackageDB]
    , usePackageIndex = Nothing
    , useDependencies = []
    , useDependenciesExclusive = False
    , useVersionMacros = False
    , useProgramDb = emptyProgramDb
    , useDistPref = defaultDistPref
    , useLoggingHandle = Nothing
    , useWorkingDir = Nothing
    , useExtraPathEnv = []
    , useExtraEnvOverrides = []
    , useWin32CleanHack = False
    , setupCacheLock = Nothing
    , isInteractive = False
    , isMainLibOrExeComponent = True
    }

workingDir :: SetupScriptOptions -> FilePath
workingDir options = case useWorkingDir options of
  Just dir
    | let fp = getSymbolicPath dir
    , not $ null fp
    -> fp
  _ -> "."

-- | A @SetupRunner@ implements a 'SetupMethod'.
type SetupRunner kind =
  Verbosity
  -> SetupScriptOptions
  -> BuildType
  -> [String]
  -> SetupRunnerArgs kind
  -> IO (SetupRunnerRes kind)

-- | Prepare to build a package by configuring a 'SetupMethod'. The returned
-- 'Setup' object identifies the method. The 'SetupScriptOptions' may be changed
-- during the configuration process; the final values are given by
-- 'setupScriptOptions'.
getSetup
  :: Verbosity
  -> SetupScriptOptions
  -> Maybe PackageDescription
  -> AllowInLibrary
  -> IO ASetup
getSetup verbosity options mpkg allowInLibrary = do
  pkg <- maybe getPkg return mpkg
  let options' =
        options
          { useCabalVersion =
              intersectVersionRanges
                (useCabalVersion options)
                (orLaterVersion (mkVersion (cabalSpecMinimumLibraryVersion (specVersion pkg))))
          }
      -- We retain Configure only for the main library and executable components.
      -- For other components, we rewrite the buildType to Simple to skip the
      -- configure step.  This is because the configure step is not supported for
      -- other components.  Configure can only impact MainLib and Exe through
      -- .buildinfo files.
      buildType' = case (buildType pkg, isMainLibOrExeComponent options) of
        (Configure, False) -> Simple
        (bt, _) -> bt
  withSetupMethod verbosity options' pkg buildType' allowInLibrary $
    \ (version, method, options'') ->
        ASetup $ Setup
          { setupMethod = method
          , setupScriptOptions = options''
          , setupVersion = version
          , setupBuildType = buildType'
          , setupPackage = pkg
          }
  where
    mbWorkDir = useWorkingDir options
    getPkg =
      (relativeSymbolicPath <$> tryFindPackageDesc verbosity mbWorkDir)
        >>= readGenericPackageDescription verbosity mbWorkDir
        >>= return . packageDescription

-- | Decide if we're going to be able to do a direct internal call to the
-- entry point in the Cabal library or if we're going to have to compile
-- and execute an external Setup.hs script.
withSetupMethod
  :: Verbosity
  -> SetupScriptOptions
  -> PackageDescription
  -> BuildType
  -> AllowInLibrary
  -> ( forall kind. (Version, SetupMethod kind, SetupScriptOptions ) -> r )
  -> IO r
withSetupMethod verbosity options pkg buildType' allowInLibrary with
  | buildType' == Custom
      || (buildType' == Hooks && isJust (useLoggingHandle options))
      || maybe False (cabalVersion /=) (useCabalSpecVersion options)
      || not (cabalVersion `withinRange` useCabalVersion options)
      || allowInLibrary == Don'tAllowInLibrary =
      withExternalSetupMethod
  | -- TODO: once we refactor the Cabal library to be able to take a logging
    -- handle as an argument, we will be able to get rid of the self-exec method.
    -- Tracking ticket: #9987.
    isJust (useLoggingHandle options) =
      return $ with (cabalVersion, SelfExecMethod, options)
  | otherwise
  = do
    abiOK <-
      if buildType' == Hooks
      then do
        -- Compile the hooks executable
        compileExternalExe verbosity options pkg buildType' WantHooks
        externalHooksABI <- externalSetupHooksABI $ hooksProgFilePath (useWorkingDir options) (useDistPref options)
        let internalHooksABI = hooksVersion
        return $ externalHooksABI == internalHooksABI
      else return True
    if abiOK
    then do
      debug verbosity $ "Using in-library setup method with build-type " ++ show buildType'
      return $ with (cabalVersion, LibraryMethod, options)
    else do
      debug verbosity $ "Hooks ABI mismatch; falling back to external setup method."
      withExternalSetupMethod
  where
    withExternalSetupMethod = do
      debug verbosity $ "Using external setup method with build-type " ++ show buildType'
      debug verbosity $
        "Using explicit dependencies: "
          ++ show (useDependenciesExclusive options)
      with <$> compileExternalExe verbosity options pkg buildType' WantSetup

runSetupMethod :: WithCallStack (SetupMethod GeneralSetup -> SetupRunner UseGeneralSetup)
runSetupMethod (ExternalMethod path) = externalSetupMethod path
runSetupMethod SelfExecMethod = selfExecSetupMethod

-- | Run a configured 'Setup' with specific arguments.
runSetup
  :: Verbosity
  -> Setup GeneralSetup
  -> [String]
  -- ^ command-line arguments
  -> SetupRunnerArgs UseGeneralSetup
  -> IO (SetupRunnerRes UseGeneralSetup)
runSetup verbosity setup args0 setupArgs = do
  let method = setupMethod setup
      options = setupScriptOptions setup
      bt = setupBuildType setup
      args = verbosityHack (setupVersion setup) args0
  when (verbosity >= deafening {- avoid test if not debug -} && args /= args0) $
    infoNoWrap verbose $
      "Applied verbosity hack:\n"
        ++ "  Before: "
        ++ show args0
        ++ "\n"
        ++ "  After:  "
        ++ show args
        ++ "\n"
  runSetupMethod method verbosity options bt args setupArgs

-- | This is a horrible hack to make sure passing fancy verbosity
-- flags (e.g., @-v'info +callstack'@) doesn't break horribly on
-- old Setup.  We can't do it in 'filterConfigureFlags' because
-- verbosity applies to ALL commands.
verbosityHack :: Version -> [String] -> [String]
verbosityHack ver args0
  | ver >= mkVersion [2, 1] = args0
  | otherwise = go args0
  where
    go (('-' : 'v' : rest) : args)
      | Just rest' <- munch rest = ("-v" ++ rest') : go args
    go (('-' : '-' : 'v' : 'e' : 'r' : 'b' : 'o' : 's' : 'e' : '=' : rest) : args)
      | Just rest' <- munch rest = ("--verbose=" ++ rest') : go args
    go ("--verbose" : rest : args)
      | Just rest' <- munch rest = "--verbose" : rest' : go args
    go rest@("--" : _) = rest
    go (arg : args) = arg : go args
    go [] = []

    munch rest =
      case runReadE flagToVerbosity rest of
        Right v
          | ver < mkVersion [2, 0]
          , verboseHasFlags v ->
              -- We could preserve the prefix, but since we're assuming
              -- it's Cabal's verbosity flag, we can assume that
              -- any format is OK
              Just (showForCabal (verboseNoFlags v))
          | ver < mkVersion [2, 1]
          , isVerboseTimestamp v ->
              -- +timestamp wasn't yet available in Cabal-2.0.0
              Just (showForCabal (verboseNoTimestamp v))
        _ -> Nothing

-- | Run a command through a configured 'Setup'.
runSetupCommand
  :: Verbosity
  -> Setup GeneralSetup
  -> CommandUI flags
  -- ^ command definition
  -> (flags -> CommonSetupFlags)
  -> flags
  -- ^ command flags
  -> [String]
  -- ^ extra command-line arguments
  -> SetupRunnerArgs UseGeneralSetup
  -> IO (SetupRunnerRes UseGeneralSetup)
runSetupCommand verbosity setup cmd getCommonFlags flags extraArgs setupArgs =
  -- The 'setupWorkingDir' flag corresponds to a global argument which needs to
  -- be passed before the individual command (e.g. 'configure' or 'build').
  let common = getCommonFlags flags
      globalFlags = mempty { globalWorkingDir = setupWorkingDir common }
      args = commandShowOptions (globalCommand []) globalFlags
          ++ (commandName cmd : commandShowOptions cmd flags ++ extraArgs)
  in runSetup verbosity setup args setupArgs

-- | Configure a 'Setup' and run a command in one step. The command flags
-- may depend on the Cabal library version in use.
setupWrapper
  :: forall setupSpec flags
  .  RightFlagsForPhase flags setupSpec
  => Verbosity
  -> SetupScriptOptions
  -> Maybe PackageDescription
  -> CommandUI flags
  -> (flags -> CommonSetupFlags)
  -> (Version -> IO flags)
  -- ^ produce command flags given the Cabal library version
  -> (Version -> [String])
  -> SetupRunnerArgs setupSpec
  -> IO (SetupRunnerRes setupSpec)
setupWrapper verbosity options mpkg cmd getCommonFlags getFlags getExtraArgs wrapperArgs = do
  let allowInLibrary = case wrapperArgs of
        NotInLibrary -> Don'tAllowInLibrary
        InLibraryArgs {} -> AllowInLibrary
  ASetup (setup :: Setup kind) <- getSetup verbosity options mpkg allowInLibrary
  let version = setupVersion setup
  flags <- getFlags version
  let extraArgs = getExtraArgs version
      notInLibraryMethod :: kind ~ GeneralSetup => IO (SetupRunnerRes setupSpec)
      notInLibraryMethod = do
        runSetupCommand verbosity setup cmd getCommonFlags flags extraArgs NotInLibrary
        return $ case wrapperArgs of
         NotInLibrary -> ()
         InLibraryArgs libArgs ->
           case libArgs of
             InLibraryConfigureArgs {} -> NotInLibraryNoLBI
             InLibraryPostConfigureArgs sPhase _ ->
               case sPhase of
                 SBuildPhase    -> []
                 SHaddockPhase  -> []
                 SReplPhase     -> ()
                 SCopyPhase     -> ()
                 SRegisterPhase -> ()
                 STestPhase     -> ()
                 SBenchPhase    -> ()
  case setupMethod setup of
    LibraryMethod ->
      case wrapperArgs of
        InLibraryArgs libArgs ->
          case libArgs of
            InLibraryConfigureArgs elabSharedConfig elabReadyPkg -> do
              -- See (1)(a) in Note [Constructing the ProgramDb]
              baseProgDb <-
                prependProgramSearchPath verbosity
                  (useExtraPathEnv options)
                  (useExtraEnvOverrides options) =<<
                  mkProgramDb flags -- Passes user-supplied arguments to e.g. GHC
                    (restoreProgramDb builtinPrograms $
                      useProgramDb options) -- Recall that 'useProgramDb' is set to 'pkgConfigCompilerProgs'
              -- See (2) in Note [Constructing the ProgramDb]
              setupProgDb <-
                configCompilerProgDb
                  verbosity
                  (pkgConfigCompiler elabSharedConfig)
                  baseProgDb
                  Nothing -- we use configProgramPaths instead
              lbi0 <-
                InLibrary.configure
                  (InLibrary.libraryConfigureInputsFromElabPackage
                     (setupBuildType setup)
                     setupProgDb
                     elabSharedConfig
                     elabReadyPkg
                     extraArgs
                  )
                  flags
              let progs0 = LBI.withPrograms lbi0
              -- See (1)(b) in Note [Constructing the ProgramDb]
              progs1 <- updatePathProgDb verbosity progs0
              let
                  lbi =
                    lbi0
                      { LBI.withPrograms = progs1
                      }
                  mbWorkDir = useWorkingDir options
                  distPref = useDistPref options
              -- Write the LocalBuildInfo to disk. This is needed, for instance, if we
              -- skip re-configuring; we retrieve the LocalBuildInfo stored on disk from
              -- the previous invocation of 'configure' and pass it to 'build'.
              writePersistBuildConfig mbWorkDir distPref lbi
              return (InLibraryLBI lbi)
            InLibraryPostConfigureArgs sPhase mbLBI ->
              case mbLBI of
                NotInLibraryNoLBI ->
                  error "internal error: in-library post-conf but no LBI"
                  -- To avoid running into the above error, we must ensure that
                  -- when we skip re-configuring, we retrieve the cached
                  -- LocalBuildInfo (see "whenReconfigure"
                  --   in Distribution.Client.ProjectBuilding.UnpackedPackage).
                InLibraryLBI lbi ->
                  case sPhase of
                    SBuildPhase    -> InLibrary.build    flags lbi extraArgs
                    SHaddockPhase  -> InLibrary.haddock  flags lbi extraArgs
                    SReplPhase     -> InLibrary.repl     flags lbi extraArgs
                    SCopyPhase     -> InLibrary.copy     flags lbi extraArgs
                    STestPhase     -> InLibrary.test     flags lbi extraArgs
                    SBenchPhase    -> InLibrary.bench    flags lbi extraArgs
                    SRegisterPhase -> InLibrary.register flags lbi extraArgs
        NotInLibrary ->
          error "internal error: NotInLibrary argument but getSetup chose InLibrary"
    ExternalMethod {} -> notInLibraryMethod
    SelfExecMethod -> notInLibraryMethod

{- Note [Constructing the ProgramDb]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When using the in-library method for configuring a package, we want to start off
with the information cabal-install already has in hand, such as the compiler.
Specifically, we skip 'Cabal.Distribution.Simple.preConfigurePackage', which
includes the call to 'configCompilerEx'.

To obtain a program database with all the required information, we do a few
things:

  (1)
    (a) When building a package with internal build tools, we must ensure that
        these build tools are available in PATH, with appropriate environment
        variable overrides for their data directory. To do this, we call
        'prependProgramSearchPath'.

    (b) Moreover, these programs must be available in the search paths for the
        compiler itself, in case they are run at compile-time (e.g. with a Template
        Haskell splice). We achieve this using 'updatePathProgDb'.

  (2) Given the compiler, we must compute the ProgramDb of programs that are
      specified alongside the compiler, such as ghc-pkg, haddock, and toolchain
      programs such as ar, ld.

      We do this using the function 'configCompilerProgDb'.
-}

-- ------------------------------------------------------------

-- * 'invoke' function

-- ------------------------------------------------------------

invoke :: Verbosity -> FilePath -> [String] -> SetupScriptOptions -> IO ()
invoke verbosity path args options = do
  info verbosity $ unwords (path : args)
  case useLoggingHandle options of
    Nothing -> return ()
    Just logHandle -> info verbosity $ "Redirecting build log to " ++ show logHandle

  progDb <- prependProgramSearchPath verbosity (useExtraPathEnv options) (useExtraEnvOverrides options) (useProgramDb options)

  searchpath <-
    programSearchPathAsPATHVar $ getProgramSearchPath progDb

  env <-
    getEffectiveEnvironment $
      [ ("PATH", Just searchpath)
      , ("HASKELL_DIST_DIR", Just (getSymbolicPath $ useDistPref options))
      ]
        ++ progOverrideEnv progDb

  let loggingHandle = case useLoggingHandle options of
        Nothing -> Inherit
        Just hdl -> UseHandle hdl
      cp =
        (proc path args)
          { Process.cwd = fmap getSymbolicPath $ useWorkingDir options
          , Process.env = env
          , Process.std_out = loggingHandle
          , Process.std_err = loggingHandle
          , Process.delegate_ctlc = isInteractive options
          }
  maybeExit $ rawSystemProc verbosity cp

-- ------------------------------------------------------------

-- * Self-Exec SetupMethod

-- ------------------------------------------------------------

selfExecSetupMethod :: SetupRunner UseGeneralSetup
selfExecSetupMethod verbosity options bt args0 NotInLibrary = do
  let args =
        [ "act-as-setup"
        , "--build-type=" ++ prettyShow bt
        , "--"
        ]
          ++ args0
  info verbosity $
    "Using self-exec internal setup method with build-type "
      ++ show bt
      ++ " and args:\n  "
      ++ show args
  path <- getExecutablePath
  invoke verbosity path args options

-- ------------------------------------------------------------

-- * External SetupMethod

-- ------------------------------------------------------------

externalSetupMethod :: WithCallStack (FilePath -> SetupRunner UseGeneralSetup)
externalSetupMethod path verbosity options _ args NotInLibrary =
#ifndef mingw32_HOST_OS
  invoke
    verbosity
    path
    args
    options
#else
    -- See 'Note: win32 clean hack' above.
    if useWin32CleanHack options
      then invokeWithWin32CleanHack path
      else invoke' path
  where
    invoke' p = invoke verbosity p args options

    invokeWithWin32CleanHack origPath = do
      info verbosity $ "Using the Win32 clean hack."
      -- Recursively removes the temp dir on exit.
      withTempDirectory verbosity (workingDir options) "cabal-tmp" $ \tmpDir ->
        bracket
          (moveOutOfTheWay tmpDir origPath)
          (\tmpPath -> maybeRestore origPath tmpPath)
          (\tmpPath -> invoke' tmpPath)

    moveOutOfTheWay tmpDir origPath = do
      let tmpPath = tmpDir </> takeFileName origPath
      Win32.moveFile origPath tmpPath
      return tmpPath

    maybeRestore origPath tmpPath = do
      let origPathDir = takeDirectory origPath
      origPathDirExists <- doesDirectoryExist origPathDir
      -- 'setup clean' didn't complete, 'dist/setup' still exists.
      when origPathDirExists $
        Win32.moveFile tmpPath origPath

#endif

data ExternalExe = HooksExe | SetupExe
data WantedExternalExe (meth :: ExternalExe) where
  WantHooks :: WantedExternalExe HooksExe
  WantSetup :: WantedExternalExe SetupExe

compileExternalExe
  :: Verbosity
  -> SetupScriptOptions
  -> PackageDescription
  -> BuildType
  -> WantedExternalExe exe
  -> IO (If (exe == HooksExe) () (Version, SetupMethod GeneralSetup, SetupScriptOptions))
compileExternalExe verbosity options pkg bt wantedMeth = do
  createDirectoryIfMissingVerbose verbosity True $ i (setupDir options)
  (cabalLibVersion, mCabalLibInstalledPkgId, options') <- cabalLibVersionToUse
  debug verbosity $ "Using Cabal library version " ++ prettyShow cabalLibVersion
  exePath <-
    if useCachedSetupExecutable
      then
        getCachedSetupExecutable
          verbosity
          platform
          (package pkg)
          bt
          options'
          cabalLibVersion
          mCabalLibInstalledPkgId
      else
        compileExe
          verbosity
          platform
          (package pkg)
          bt
          wantedMeth
          options'
          cabalLibVersion
          mCabalLibInstalledPkgId
          False

  -- Since useWorkingDir can change the relative path, the path argument must
  -- be turned into an absolute path. On some systems, runProcess' will take
  -- path as relative to the new working directory instead of the current
  -- working directory.
  exePath' <- tryCanonicalizePath exePath

  -- See 'Note: win32 clean hack' above.
#ifdef mingw32_HOST_OS
  -- setupProgFile may not exist if we're using a cached program
  setupProgFile' <- canonicalizePathNoThrow $ i (setupProgFile options)
  let win32CleanHackNeeded =
        (useWin32CleanHack options)
          -- Skip when a cached setup script is used.
          && setupProgFile' `equalFilePath` exePath'
#else
  let win32CleanHackNeeded = False
#endif
  let options'' = options'{useWin32CleanHack = win32CleanHackNeeded}

  case wantedMeth of
    WantHooks -> return ()
    WantSetup -> return (cabalLibVersion, ExternalMethod exePath', options'')
  where
    mbWorkDir = useWorkingDir options
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i :: SymbolicPathX allowAbs Pkg to -> FilePath
    i = interpretSymbolicPath mbWorkDir
    platform = fromMaybe buildPlatform (usePlatform options)

    useCachedSetupExecutable =
      bt == Simple || bt == Configure || bt == Make

    -- Choose the version of Cabal to use if the setup script has a dependency on
    -- Cabal, and possibly update the setup script options. The version also
    -- determines how to filter the flags to Setup.
    --
    -- We first check whether the dependency solver has specified a Cabal version.
    -- If it has, we use the solver's version without looking at the installed
    -- package index (See issue #3436). Otherwise, we pick the Cabal version by
    -- checking 'useCabalSpecVersion', then the saved version, and finally the
    -- versions available in the index.
    --
    -- The version chosen here must match the one used in 'compileExe'
    -- (See issue #3433).
    cabalLibVersionToUse
      :: IO
          ( Version
          , Maybe ComponentId
          , SetupScriptOptions
          )
    cabalLibVersionToUse =
      case find (isCabalPkgId . snd) (useDependencies options) of
        Just (unitId, pkgId) -> do
          let version = pkgVersion pkgId
          updateSetupScript version bt
          writeSetupVersionFile version
          return (version, Just unitId, options)
        Nothing ->
          case useCabalSpecVersion options of
            Just version -> do
              updateSetupScript version bt
              writeSetupVersionFile version
              return (version, Nothing, options)
            Nothing -> do
              savedVer <- savedVersion
              case savedVer of
                Just version | version `withinRange` useCabalVersion options ->
                  do
                    updateSetupScript version bt
                    -- Does the previously compiled setup executable
                    -- still exist and is it up-to date?
                    useExisting <- canUseExistingSetup version
                    if useExisting
                      then return (version, Nothing, options)
                      else installedVersion
                _ -> installedVersion
      where
        -- This check duplicates the checks in 'getCachedSetupExecutable' /
        -- 'compileExe'. Unfortunately, we have to perform it twice
        -- because the selected Cabal version may change as a result of this
        -- check.
        canUseExistingSetup :: Version -> IO Bool
        canUseExistingSetup version =
          if useCachedSetupExecutable
            then do
              (_, cachedSetupProgFile) <- cachedSetupDirAndProg platform bt options version
              doesFileExist cachedSetupProgFile
            else
              (&&)
                <$> i (setupProgFile options) `existsAndIsMoreRecentThan` i (setupHs options)
                <*> i (setupProgFile options) `existsAndIsMoreRecentThan` i (setupVersionFile options)

        writeSetupVersionFile :: Version -> IO ()
        writeSetupVersionFile version =
          writeFile (i (setupVersionFile options)) (show version ++ "\n")

        installedVersion
          :: IO
              ( Version
              , Maybe InstalledPackageId
              , SetupScriptOptions
              )
        installedVersion = do
          (comp, progdb, options') <- configureCompiler verbosity options
          (version, mipkgid, options'') <-
            installedCabalVersion
              verbosity
              pkg
              bt
              options'
              comp
              progdb
          updateSetupScript version bt
          writeSetupVersionFile version
          return (version, mipkgid, options'')

        savedVersion :: IO (Maybe Version)
        savedVersion = do
          versionString <- readFile (i (setupVersionFile options)) `catchIO` \_ -> return ""
          case reads versionString of
            [(version, s)] | all isSpace s -> return (Just version)
            _ -> return Nothing

    -- \| Update a Setup.hs script, creating it if necessary.
    updateSetupScript :: Version -> BuildType -> IO ()
    updateSetupScript _ Custom = do
      useHs <- doesFileExist customSetupHs
      useLhs <- doesFileExist customSetupLhs
      unless (useHs || useLhs) $
        dieWithException verbosity UpdateSetupScript
      let src = (if useHs then customSetupHs else customSetupLhs)
      srcNewer <- src `moreRecentFile` i (setupHs options)
      when srcNewer $
        if useHs
          then copyFileVerbose verbosity src (i (setupHs options))
          else runSimplePreProcessor ppUnlit src (i (setupHs options)) verbosity
      where
        customSetupHs = workingDir options </> "Setup.hs"
        customSetupLhs = workingDir options </> "Setup.lhs"
    updateSetupScript cabalLibVersion Hooks = do
      let customSetupHooks = workingDir options </> "SetupHooks.hs"
      useHs <- doesFileExist customSetupHooks
      unless useHs $
        die'
          verbosity
          "Using 'build-type: Hooks' but there is no SetupHooks.hs file."
      copyFileVerbose verbosity customSetupHooks (i (setupHooks options))
      rewriteFileLBS verbosity (i (setupHs options)) (buildTypeScript Hooks cabalLibVersion)
      rewriteFileLBS verbosity (i (hooksHs options)) hooksExeScript
    updateSetupScript cabalLibVersion bt' =
      rewriteFileLBS verbosity (i (setupHs options)) (buildTypeScript bt' cabalLibVersion)

-- | The source code for a non-Custom 'Setup' executable.
buildTypeScript :: BuildType -> Version -> BS.ByteString
buildTypeScript bt cabalLibVersion = "{-# LANGUAGE NoImplicitPrelude #-}\n" <> case bt of
  Simple -> "import Distribution.Simple; main = defaultMain\n"
  Configure
    | cabalLibVersion >= mkVersion [3, 13, 0]
    -> "import Distribution.Simple; main = defaultMainWithSetupHooks autoconfSetupHooks\n"
    | cabalLibVersion >= mkVersion [1, 3, 10]
    -> "import Distribution.Simple; main = defaultMainWithHooks autoconfUserHooks\n"
    | otherwise
    -> "import Distribution.Simple; main = defaultMainWithHooks defaultUserHooks\n"
  Make -> "import Distribution.Make; main = defaultMain\n"
  Hooks
    | cabalLibVersion >= mkVersion [3, 13, 0]
    -> "import Distribution.Simple; import SetupHooks; main = defaultMainWithSetupHooks setupHooks\n"
    | otherwise
    -> error "buildTypeScript Hooks with Cabal < 3.13"
  Custom -> error "buildTypeScript Custom"

-- | The source code for an external hooks executable, using the 'hooks-exe' library.
hooksExeScript :: BS.ByteString
hooksExeScript =
  "{-# LANGUAGE NoImplicitPrelude #-}\nimport Distribution.Client.SetupHooks.HooksExe (hooksMain); import SetupHooks; main = hooksMain setupHooks\n"

installedCabalVersion
  :: Verbosity
  -> PackageDescription
  -> BuildType
  -> SetupScriptOptions
  -> Compiler
  -> ProgramDb
  -> IO
      ( Version
      , Maybe InstalledPackageId
      , SetupScriptOptions
      )
installedCabalVersion _verbosity pkg bt options' _ _
  | packageName pkg == mkPackageName "Cabal"
      && bt == Custom =
      return (packageVersion pkg, Nothing, options')
installedCabalVersion verbosity pkg _bt options' compiler progdb = do
  index <- maybeGetInstalledPackages verbosity options' compiler progdb
  let cabalDepName = mkPackageName "Cabal"
      cabalDepVersion = useCabalVersion options'
      options'' = options'{usePackageIndex = Just index}
  case PackageIndex.lookupDependency index cabalDepName cabalDepVersion of
    [] ->
      dieWithException verbosity $ InstalledCabalVersion (packageName pkg) (useCabalVersion options')
    pkgs ->
      let ipkginfo = fromMaybe err $ safeHead . snd . bestVersion fst $ pkgs
          err = error "Distribution.Client.installedCabalVersion: empty version list"
       in return
            ( packageVersion ipkginfo
            , Just . IPI.installedComponentId $ ipkginfo
            , options''
            )

bestVersion :: (a -> Version) -> [a] -> a
bestVersion f = firstMaximumBy (comparing (preference . f))
  where
    -- Like maximumBy, but picks the first maximum element instead of the
    -- last. In general, we expect the preferred version to go first in the
    -- list. For the default case, this has the effect of choosing the version
    -- installed in the user package DB instead of the global one. See #1463.
    --
    -- Note: firstMaximumBy could be written as just
    -- `maximumBy cmp . reverse`, but the problem is that the behaviour of
    -- maximumBy is not fully specified in the case when there is not a single
    -- greatest element.
    firstMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    firstMaximumBy _ [] =
      error "Distribution.Client.firstMaximumBy: empty list"
    firstMaximumBy cmp xs = foldl1' maxBy xs
      where
        maxBy x y = case cmp x y of GT -> x; EQ -> x; LT -> y

    preference version =
      ( sameVersion
      , sameMajorVersion
      , stableVersion
      , latestVersion
      )
      where
        sameVersion = version == cabalVersion
        sameMajorVersion = majorVersion version == majorVersion cabalVersion
        majorVersion = take 2 . versionNumbers
        stableVersion = case versionNumbers version of
          (_ : x : _) -> even x
          _ -> False
        latestVersion = version

configureCompiler
  :: Verbosity
  -> SetupScriptOptions
  -> IO (Compiler, ProgramDb, SetupScriptOptions)
configureCompiler verbosity options' = do
  (comp, progdb) <- case useCompiler options' of
    Just comp -> return (comp, useProgramDb options')
    Nothing -> do
      (comp, _, progdb) <-
        configCompilerEx
          (Just GHC)
          Nothing
          Nothing
          (useProgramDb options')
          verbosity
      return (comp, progdb)
  -- Whenever we need to call configureCompiler, we also need to access the
  -- package index, so let's cache it in SetupScriptOptions.
  index <- maybeGetInstalledPackages verbosity options' comp progdb
  return
    ( comp
    , progdb
    , options'
        { useCompiler = Just comp
        , usePackageIndex = Just index
        , useProgramDb = progdb
        }
    )

maybeGetInstalledPackages
  :: Verbosity
  -> SetupScriptOptions
  -> Compiler
  -> ProgramDb
  -> IO InstalledPackageIndex
maybeGetInstalledPackages verbosity options' comp progdb =
  case usePackageIndex options' of
    Just index -> return index
    Nothing ->
      getInstalledPackages
        verbosity
        comp
        (usePackageDB options')
        progdb

-- | Path to the setup exe cache directory and path to the cached setup
-- executable.
cachedSetupDirAndProg
  :: Platform
  -> BuildType
  -> SetupScriptOptions
  -> Version
  -> IO (FilePath, FilePath)
cachedSetupDirAndProg platform bt options' cabalLibVersion = do
  cacheDir <- defaultCacheDir
  let setupCacheDir = cacheDir </> "setup-exe-cache"
      cachedSetupProgFile =
        setupCacheDir
          </> ( "setup-"
                  ++ buildTypeString
                  ++ "-"
                  ++ cabalVersionString
                  ++ "-"
                  ++ platformString
                  ++ "-"
                  ++ compilerVersionString
              )
          <.> exeExtension buildPlatform
  return (setupCacheDir, cachedSetupProgFile)
  where
    buildTypeString = show bt
    cabalVersionString = "Cabal-" ++ prettyShow cabalLibVersion
    compilerVersionString =
      prettyShow $
        maybe buildCompilerId compilerId $
          useCompiler options'
    platformString = prettyShow platform

-- | Look up the executable in the cache; update the cache if the executable
-- is not found.
getCachedSetupExecutable
  :: Verbosity
  -> Platform
  -> PackageIdentifier
  -> BuildType
  -> SetupScriptOptions
  -> Version
  -> Maybe InstalledPackageId
  -> IO FilePath
getCachedSetupExecutable
  verbosity
  platform
  pkgId
  bt
  options'
  cabalLibVersion
  maybeCabalLibInstalledPkgId = do
    (setupCacheDir, cachedSetupProgFile) <-
      cachedSetupDirAndProg platform bt options' cabalLibVersion
    cachedSetupExists <- doesFileExist cachedSetupProgFile
    if cachedSetupExists
      then
        debug verbosity $
          "Found cached setup executable: " ++ cachedSetupProgFile
      else criticalSection' $ do
        -- The cache may have been populated while we were waiting.
        cachedSetupExists' <- doesFileExist cachedSetupProgFile
        if cachedSetupExists'
          then
            debug verbosity $
              "Found cached setup executable: " ++ cachedSetupProgFile
          else do
            debug verbosity $ "Setup executable not found in the cache."
            src <-
              compileExe
                verbosity
                platform
                pkgId
                bt
                WantSetup
                options'
                cabalLibVersion
                maybeCabalLibInstalledPkgId
                True
            createDirectoryIfMissingVerbose verbosity True setupCacheDir
            installExecutableFile verbosity src cachedSetupProgFile
            -- Do not strip if we're using GHCJS, since the result may be a script
            when (maybe True ((/= GHCJS) . compilerFlavor) $ useCompiler options') $ do
              -- Add the relevant PATH overrides for the package to the
              -- program database.
              setupProgDb
                <- prependProgramSearchPath verbosity
                      (useExtraPathEnv options')
                      (useExtraEnvOverrides options')
                      (useProgramDb options')
                     >>= configureAllKnownPrograms verbosity
              Strip.stripExe
                verbosity
                platform
                setupProgDb
                cachedSetupProgFile
    return cachedSetupProgFile
    where
      criticalSection' = maybe id criticalSection $ setupCacheLock options'

-- | If the Setup.hs is out of date wrt the executable then recompile it.
-- Currently this is GHC/GHCJS only. It should really be generalised.
compileExe
  :: Verbosity
  -> Platform
  -> PackageIdentifier
  -> BuildType
  -> WantedExternalExe exe
  -> SetupScriptOptions
  -> Version
  -> Maybe ComponentId
  -> Bool
  -> IO FilePath
compileExe verbosity platform pkgId bt wantedMeth opts ver mbCompId forceCompile =
  case wantedMeth of
    WantHooks ->
      compileHooksScript verbosity platform pkgId opts ver mbCompId forceCompile
    WantSetup ->
      compileSetupScript verbosity platform pkgId bt opts ver mbCompId forceCompile

compileSetupScript
  :: Verbosity
  -> Platform
  -> PackageIdentifier
  -> BuildType
  -> SetupScriptOptions
  -> Version
  -> Maybe ComponentId
  -> Bool
  -> IO FilePath
compileSetupScript verbosity platform pkgId bt opts ver mbCompId forceCompile =
  compileSetupX "Setup"
    [setupHs opts] (setupProgFile opts)
    verbosity platform pkgId bt opts ver mbCompId forceCompile

compileHooksScript
  :: Verbosity
  -> Platform
  -> PackageIdentifier
  -> SetupScriptOptions
  -> Version
  -> Maybe ComponentId
  -> Bool
  -> IO FilePath
compileHooksScript verbosity platform pkgId opts ver mbCompId forceCompile =
  compileSetupX "SetupHooks"
    [setupHooks opts, hooksHs opts] (hooksProgFile opts)
    verbosity platform pkgId Hooks opts ver mbCompId forceCompile

setupDir :: SetupScriptOptions -> SymbolicPath Pkg (Dir setup)
setupDir opts = useDistPref opts Cabal.Path.</> makeRelativePathEx "setup"
setupVersionFile :: SetupScriptOptions -> SymbolicPath Pkg File
setupVersionFile opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "setup" <.> "version" )
setupHs, hooksHs, setupHooks, setupProgFile, hooksProgFile :: SetupScriptOptions -> SymbolicPath Pkg File
setupHs opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "setup" <.> "hs" )
hooksHs opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "hooks" <.> "hs" )
setupHooks opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "SetupHooks" <.> "hs" )
setupProgFile opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "setup" <.> exeExtension buildPlatform )
hooksProgFile opts = setupDir opts Cabal.Path.</> makeRelativePathEx ( "hooks" <.> exeExtension buildPlatform )

compileSetupX
  :: String
  -> [SymbolicPath Pkg File] -- input files
  -> SymbolicPath Pkg File   -- output file
  -> Verbosity
  -> Platform
  -> PackageIdentifier
  -> BuildType
  -> SetupScriptOptions
  -> Version
  -> Maybe ComponentId
  -> Bool
  -> IO FilePath
compileSetupX
  what
  inPaths outPath
  verbosity
  platform
  pkgId
  bt
  options'
  cabalLibVersion
  maybeCabalLibInstalledPkgId
  forceCompile = do
    setupXHsNewer <- fmap or $ sequenceA $ fmap ( \ inPath -> i inPath `moreRecentFile` i outPath ) inPaths
    cabalVersionNewer <- i (setupVersionFile options') `moreRecentFile` i (setupProgFile options')
    let outOfDate = setupXHsNewer || cabalVersionNewer
    when (outOfDate || forceCompile) $ do
      debug verbosity $ what ++ " executable needs to be updated, compiling..."
      (compiler, progdb, options'') <- configureCompiler verbosity options'
      pkgDbs <- traverse (traverse (makeRelativeToDirS mbWorkDir)) (coercePackageDBStack (usePackageDB options''))
      let cabalPkgid = PackageIdentifier (mkPackageName "Cabal") cabalLibVersion
          (program, extraOpts) =
            case compilerFlavor compiler of
              GHCJS -> (ghcjsProgram, ["-build-runner"])
              _ -> (ghcProgram, ["-threaded"])
          cabalDep =
            maybe
              []
              (\ipkgid -> [(ipkgid, cabalPkgid)])
              maybeCabalLibInstalledPkgId

          -- With 'useDependenciesExclusive' and Custom build type,
          -- we enforce the deps specified, so only the given ones can be used.
          -- Otherwise we add on a dep on the Cabal library
          -- (unless 'useDependencies' already contains one).
          selectedDeps
            |  (useDependenciesExclusive options' && (bt /= Hooks))
            -- NB: to compile build-type: Hooks packages, we need Cabal
            -- in order to compile @main = defaultMainWithSetupHooks setupHooks@.
            || any (isCabalPkgId . snd) (useDependencies options')
            = useDependencies options'
            | otherwise =
                useDependencies options' ++ cabalDep
          addRenaming (ipid, _) =
            -- Assert 'DefUnitId' invariant
            ( Backpack.DefiniteUnitId (unsafeMkDefUnitId (newSimpleUnitId ipid))
            , defaultRenaming
            )
          cppMacrosFile = setupDir options' Cabal.Path.</> makeRelativePathEx "setup_macros.h"
          ghcOptions =
            mempty
              { -- Respect -v0, but don't crank up verbosity on GHC if
                -- Cabal verbosity is requested. For that, use
                -- --ghc-option=-v instead!
                ghcOptVerbosity = Flag (min verbosity normal)
              , ghcOptMode = Flag GhcModeMake
              , ghcOptInputFiles = toNubListR inPaths
              , ghcOptOutputFile = Flag outPath
              , ghcOptObjDir = Flag (setupDir options')
              , ghcOptHiDir = Flag (setupDir options')
              , ghcOptSourcePathClear = Flag True
              , ghcOptSourcePath = case bt of
                  Custom -> toNubListR [sameDirectory]
                  Hooks -> toNubListR [sameDirectory]
                  _ -> mempty
              , ghcOptPackageDBs = pkgDbs
              , ghcOptHideAllPackages = Flag (useDependenciesExclusive options')
              , ghcOptCabal = Flag (useDependenciesExclusive options')
              , ghcOptPackages = toNubListR $ map addRenaming selectedDeps
              -- With 'useVersionMacros', use a version CPP macros .h file.
              , ghcOptCppIncludes =
                  toNubListR
                    [ cppMacrosFile
                    | useVersionMacros options'
                    ]
              , ghcOptExtra = extraOpts
              , ghcOptExtensions = toNubListR $
                  if bt == Custom || any (isBasePkgId . snd) selectedDeps
                  then []
                  else [ Simple.DisableExtension Simple.ImplicitPrelude ]
                    -- Pass -WNoImplicitPrelude to avoid depending on base
                    -- when compiling a Simple Setup.hs file.
              , ghcOptExtensionMap = Map.fromList . Simple.compilerExtensions $ compiler
              }
      let ghcCmdLine = renderGhcOptions compiler platform ghcOptions
      when (useVersionMacros options') $
        rewriteFileEx verbosity (i cppMacrosFile) $
          generatePackageVersionMacros (pkgVersion pkgId) (map snd selectedDeps)
      case useLoggingHandle options' of
        Nothing -> runDbProgramCwd verbosity mbWorkDir program progdb ghcCmdLine
        -- If build logging is enabled, redirect compiler output to
        -- the log file.
        Just logHandle -> do
          output <-
            getDbProgramOutputCwd
              verbosity
              mbWorkDir
              program
              progdb
              ghcCmdLine
          hPutStr logHandle output
    return $ i outPath
  where
    mbWorkDir = useWorkingDir options'
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i :: SymbolicPathX allowAbs Pkg to -> FilePath
    i = interpretSymbolicPath mbWorkDir

isCabalPkgId, isBasePkgId :: PackageIdentifier -> Bool
isCabalPkgId (PackageIdentifier pname _) = pname == mkPackageName "Cabal"
isBasePkgId (PackageIdentifier pname _) = pname == mkPackageName "base"
