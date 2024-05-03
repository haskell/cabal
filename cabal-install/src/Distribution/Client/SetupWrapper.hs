{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , setupWrapper
  , SetupScriptOptions (..)
  , defaultSetupScriptOptions
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Distribution.Backpack as Backpack
import Distribution.CabalSpecVersion (cabalSpecMinimumLibraryVersion)
import Distribution.Compiler
  ( CompilerFlavor (GHC, GHCJS)
  , buildCompilerId
  )
import qualified Distribution.Make as Make
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
  ( defaultDistPref
  , exeExtension
  )
import Distribution.Simple.Compiler
  ( Compiler (compilerId)
  , PackageDB (..)
  , PackageDBStack
  , compilerFlavor
  )
import Distribution.Simple.Configure
  ( configCompilerEx
  )
import Distribution.Simple.PackageDescription
  ( readGenericPackageDescription
  )
import Distribution.Simple.PreProcess
  ( ppUnlit
  , runSimplePreProcessor
  )
import Distribution.Simple.Program
  ( ProgramDb
  , emptyProgramDb
  , getDbProgramOutputCwd
  , getProgramSearchPath
  , ghcProgram
  , ghcjsProgram
  , runDbProgramCwd
  )
import Distribution.Simple.Program.Db
  ( configureAllKnownPrograms
  , prependProgramSearchPath
  , progOverrideEnv
  )
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
#ifdef mingw32_HOST_OS
  , canonicalizePathNoThrow
#endif
  , moreRecentFile
  , tryCanonicalizePath
  , withEnv
  , withEnvOverrides
  , withExtraPathEnv
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
import Distribution.Simple.Setup
  ( Flag (..), CommonSetupFlags (..), GlobalFlags (..)
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
import Distribution.System (Platform (..), buildPlatform)
import Distribution.Utils.NubList
  ( toNubListR
  )
import Distribution.Verbosity

import Data.List (foldl1')
import Distribution.Simple.Setup (globalCommand)
import Distribution.Client.Compat.ExecutablePath (getExecutablePath)
import Distribution.Compat.Process (proc)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import System.IO (Handle, hPutStr)
import System.Process (StdStream (..))
import qualified System.Process as Process

import qualified Data.ByteString.Lazy as BS
import Distribution.Client.Errors

#ifdef mingw32_HOST_OS
import Distribution.Simple.Utils
         ( withTempDirectory )

import Control.Exception   ( bracket )
import System.FilePath     ( equalFilePath, takeDirectory )
import System.Directory    ( doesDirectoryExist )
import qualified System.Win32 as Win32
#endif

-- | @Setup@ encapsulates the outcome of configuring a setup method to build a
-- particular package.
data Setup = Setup
  { setupMethod :: SetupMethod
  , setupScriptOptions :: SetupScriptOptions
  , setupVersion :: Version
  , setupBuildType :: BuildType
  , setupPackage :: PackageDescription
  }

-- | @SetupMethod@ represents one of the methods used to run Cabal commands.
data SetupMethod
  = -- | run Cabal commands through \"cabal\" in the
    -- current process
    InternalMethod
  | -- | run Cabal commands through \"cabal\" as a
    -- child process
    SelfExecMethod
  | -- | run Cabal commands through a custom \"Setup\" executable
    ExternalMethod FilePath

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
  , usePackageDB :: PackageDBStack
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
  , forceExternalSetupMethod :: Bool
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
    , forceExternalSetupMethod = False
    , setupCacheLock = Nothing
    , isInteractive = False
    }

workingDir :: SetupScriptOptions -> FilePath
workingDir options = case useWorkingDir options of
  Just dir
    | let fp = getSymbolicPath dir
    , not $ null fp
    -> fp
  _ -> "."

-- | A @SetupRunner@ implements a 'SetupMethod'.
type SetupRunner =
  Verbosity
  -> SetupScriptOptions
  -> BuildType
  -> [String]
  -> IO ()

-- | Prepare to build a package by configuring a 'SetupMethod'. The returned
-- 'Setup' object identifies the method. The 'SetupScriptOptions' may be changed
-- during the configuration process; the final values are given by
-- 'setupScriptOptions'.
getSetup
  :: Verbosity
  -> SetupScriptOptions
  -> Maybe PackageDescription
  -> IO Setup
getSetup verbosity options mpkg = do
  pkg <- maybe getPkg return mpkg
  let options' =
        options
          { useCabalVersion =
              intersectVersionRanges
                (useCabalVersion options)
                (orLaterVersion (mkVersion (cabalSpecMinimumLibraryVersion (specVersion pkg))))
          }
      buildType' = buildType pkg
  (version, method, options'') <-
    getSetupMethod verbosity options' pkg buildType'
  return
    Setup
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
getSetupMethod
  :: Verbosity
  -> SetupScriptOptions
  -> PackageDescription
  -> BuildType
  -> IO (Version, SetupMethod, SetupScriptOptions)
getSetupMethod verbosity options pkg buildType'
  | buildType' == Custom
      || buildType' == Hooks
      || maybe False (cabalVersion /=) (useCabalSpecVersion options)
      || not (cabalVersion `withinRange` useCabalVersion options) =
      getExternalSetupMethod verbosity options pkg buildType'
  | isJust (useLoggingHandle options)
      -- Forcing is done to use an external process e.g. due to parallel
      -- build concerns.
      || forceExternalSetupMethod options =
      return (cabalVersion, SelfExecMethod, options)
  | otherwise = return (cabalVersion, InternalMethod, options)

runSetupMethod :: WithCallStack (SetupMethod -> SetupRunner)
runSetupMethod InternalMethod = internalSetupMethod
runSetupMethod (ExternalMethod path) = externalSetupMethod path
runSetupMethod SelfExecMethod = selfExecSetupMethod

-- | Run a configured 'Setup' with specific arguments.
runSetup
  :: Verbosity
  -> Setup
  -> [String]
  -- ^ command-line arguments
  -> IO ()
runSetup verbosity setup args0 = do
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
  runSetupMethod method verbosity options bt args

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
  -> Setup
  -> CommandUI flags
  -- ^ command definition
  -> (flags -> CommonSetupFlags)
  -> flags
  -- ^ command flags
  -> [String]
  -- ^ extra command-line arguments
  -> IO ()
runSetupCommand verbosity setup cmd getCommonFlags flags extraArgs =
  -- The 'setupWorkingDir' flag corresponds to a global argument which needs to
  -- be passed before the individual command (e.g. 'configure' or 'build').
  let common = getCommonFlags flags
      globalFlags = mempty { globalWorkingDir = setupWorkingDir common }
      args = commandShowOptions (globalCommand []) globalFlags
          ++ (commandName cmd : commandShowOptions cmd flags ++ extraArgs)
  in runSetup verbosity setup args

-- | Configure a 'Setup' and run a command in one step. The command flags
-- may depend on the Cabal library version in use.
setupWrapper
  :: Verbosity
  -> SetupScriptOptions
  -> Maybe PackageDescription
  -> CommandUI flags
  -> (flags -> CommonSetupFlags)
  -> (Version -> flags)
  -- ^ produce command flags given the Cabal library version
  -> (Version -> [String])
  -> IO ()
setupWrapper verbosity options mpkg cmd getCommonFlags getFlags getExtraArgs = do
  setup <- getSetup verbosity options mpkg
  let version = setupVersion setup
      flags = getFlags version
      extraArgs = getExtraArgs version
  runSetupCommand
    verbosity
    setup
    cmd
    getCommonFlags
    flags
    extraArgs

-- ------------------------------------------------------------

-- * Internal SetupMethod

-- ------------------------------------------------------------

-- | Run a Setup script by directly invoking the @Cabal@ library.
internalSetupMethod :: SetupRunner
internalSetupMethod verbosity options bt args = do
  info verbosity $
    "Using internal setup method with build-type "
      ++ show bt
      ++ " and args:\n  "
      ++ show args
  -- NB: we do not set the working directory of the process here, because
  -- we will instead pass the -working-dir flag when invoking the Setup script.
  -- Note that the Setup script is guaranteed to support this flag, because
  -- the logic in 'getSetupMethod' guarantees we have an up-to-date Cabal version.
  --
  -- In the future, it would be desirable to also stop relying on the following
  -- pieces of process-global state, as this would allow us to use this internal
  -- setup method in concurrent contexts.
  withEnv "HASKELL_DIST_DIR" (getSymbolicPath $ useDistPref options) $
    withExtraPathEnv (useExtraPathEnv options) $
      withEnvOverrides (useExtraEnvOverrides options) $
        buildTypeAction bt args

buildTypeAction :: BuildType -> ([String] -> IO ())
buildTypeAction Simple = Simple.defaultMainArgs
buildTypeAction Configure =
  Simple.defaultMainWithHooksArgs
    Simple.autoconfUserHooks
buildTypeAction Make = Make.defaultMainArgs
buildTypeAction Hooks  = error "buildTypeAction Hooks"
buildTypeAction Custom = error "buildTypeAction Custom"

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

selfExecSetupMethod :: SetupRunner
selfExecSetupMethod verbosity options bt args0 = do
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

externalSetupMethod :: WithCallStack (FilePath -> SetupRunner)
externalSetupMethod path verbosity options _ args =
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
      let tmpPath = tmpDir </> "setup" <.> exeExtension buildPlatform
      Win32.moveFile origPath tmpPath
      return tmpPath

    maybeRestore origPath tmpPath = do
      let origPathDir = takeDirectory origPath
      origPathDirExists <- doesDirectoryExist origPathDir
      -- 'setup clean' didn't complete, 'dist/setup' still exists.
      when origPathDirExists $
        Win32.moveFile tmpPath origPath

#endif

getExternalSetupMethod
  :: Verbosity
  -> SetupScriptOptions
  -> PackageDescription
  -> BuildType
  -> IO (Version, SetupMethod, SetupScriptOptions)
getExternalSetupMethod verbosity options pkg bt = do
  debug verbosity $ "Using external setup method with build-type " ++ show bt
  debug verbosity $
    "Using explicit dependencies: "
      ++ show (useDependenciesExclusive options)
  createDirectoryIfMissingVerbose verbosity True $ i setupDir
  (cabalLibVersion, mCabalLibInstalledPkgId, options') <- cabalLibVersionToUse
  debug verbosity $ "Using Cabal library version " ++ prettyShow cabalLibVersion
  path <-
    if useCachedSetupExecutable
      then
        getCachedSetupExecutable
          options'
          cabalLibVersion
          mCabalLibInstalledPkgId
      else
        compileSetupExecutable
          options'
          cabalLibVersion
          mCabalLibInstalledPkgId
          False

  -- Since useWorkingDir can change the relative path, the path argument must
  -- be turned into an absolute path. On some systems, runProcess' will take
  -- path as relative to the new working directory instead of the current
  -- working directory.
  path' <- tryCanonicalizePath path

  -- See 'Note: win32 clean hack' above.
#ifdef mingw32_HOST_OS
  -- setupProgFile may not exist if we're using a cached program
  setupProgFile' <- canonicalizePathNoThrow $ i setupProgFile
  let win32CleanHackNeeded =
        (useWin32CleanHack options)
          -- Skip when a cached setup script is used.
          && setupProgFile' `equalFilePath` path'
#else
  let win32CleanHackNeeded = False
#endif
  let options'' = options'{useWin32CleanHack = win32CleanHackNeeded}

  return (cabalLibVersion, ExternalMethod path', options'')
  where
    mbWorkDir = useWorkingDir options
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    i = interpretSymbolicPath mbWorkDir
    setupDir = useDistPref options Cabal.Path.</> makeRelativePathEx "setup"
    setupVersionFile = setupDir Cabal.Path.</> makeRelativePathEx ("setup" <.> "version")
    setupHs = setupDir Cabal.Path.</> makeRelativePathEx ("setup" <.> "hs")
    setupHooks = setupDir Cabal.Path.</> makeRelativePathEx ("SetupHooks" <.> "hs")
    setupProgFile = setupDir Cabal.Path.</> makeRelativePathEx ("setup" <.> exeExtension buildPlatform)

    platform = fromMaybe buildPlatform (usePlatform options)

    useCachedSetupExecutable =
      bt == Simple || bt == Configure || bt == Make

    maybeGetInstalledPackages
      :: SetupScriptOptions
      -> Compiler
      -> ProgramDb
      -> IO InstalledPackageIndex
    maybeGetInstalledPackages options' comp progdb =
      case usePackageIndex options' of
        Just index -> return index
        Nothing ->
          getInstalledPackages
            verbosity
            comp
            (usePackageDB options')
            progdb

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
    -- The version chosen here must match the one used in 'compileSetupExecutable'
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
        -- 'compileSetupExecutable'. Unfortunately, we have to perform it twice
        -- because the selected Cabal version may change as a result of this
        -- check.
        canUseExistingSetup :: Version -> IO Bool
        canUseExistingSetup version =
          if useCachedSetupExecutable
            then do
              (_, cachedSetupProgFile) <- cachedSetupDirAndProg options version
              doesFileExist cachedSetupProgFile
            else
              (&&)
                <$> i setupProgFile `existsAndIsMoreRecentThan` i setupHs
                <*> i setupProgFile `existsAndIsMoreRecentThan` i setupVersionFile

        writeSetupVersionFile :: Version -> IO ()
        writeSetupVersionFile version =
          writeFile (i setupVersionFile) (show version ++ "\n")

        installedVersion
          :: IO
              ( Version
              , Maybe InstalledPackageId
              , SetupScriptOptions
              )
        installedVersion = do
          (comp, progdb, options') <- configureCompiler options
          (version, mipkgid, options'') <-
            installedCabalVersion
              options'
              comp
              progdb
          updateSetupScript version bt
          writeSetupVersionFile version
          return (version, mipkgid, options'')

        savedVersion :: IO (Maybe Version)
        savedVersion = do
          versionString <- readFile (i setupVersionFile) `catchIO` \_ -> return ""
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
      srcNewer <- src `moreRecentFile` i setupHs
      when srcNewer $
        if useHs
          then copyFileVerbose verbosity src (i setupHs)
          else runSimplePreProcessor ppUnlit src (i setupHs) verbosity
      where
        customSetupHs = workingDir options </> "Setup.hs"
        customSetupLhs = workingDir options </> "Setup.lhs"
    updateSetupScript cabalLibVersion Hooks = do

      let customSetupHooks = workingDir options </> "SetupHooks.hs"
      useHs <- doesFileExist customSetupHooks
      unless (useHs) $
        die'
          verbosity
          "Using 'build-type: Hooks' but there is no SetupHooks.hs file."
      copyFileVerbose verbosity customSetupHooks (i setupHooks)
      rewriteFileLBS verbosity (i setupHs) (buildTypeScript cabalLibVersion)
--      rewriteFileLBS verbosity hooksHs hooksScript
    updateSetupScript cabalLibVersion _ =
      rewriteFileLBS verbosity (i setupHs) (buildTypeScript cabalLibVersion)

    buildTypeScript :: Version -> BS.ByteString
    buildTypeScript cabalLibVersion = case bt of
      Simple -> "import Distribution.Simple; main = defaultMain\n"
      Configure
        | cabalLibVersion >= mkVersion [1, 3, 10] -> "import Distribution.Simple; main = defaultMainWithHooks autoconfUserHooks\n"
        | otherwise -> "import Distribution.Simple; main = defaultMainWithHooks defaultUserHooks\n"
      Make -> "import Distribution.Make; main = defaultMain\n"
      Hooks -> "import Distribution.Simple; import SetupHooks; main = defaultMainWithSetupHooks setupHooks\n"
      Custom -> error "buildTypeScript Custom"

    installedCabalVersion
      :: SetupScriptOptions
      -> Compiler
      -> ProgramDb
      -> IO
          ( Version
          , Maybe InstalledPackageId
          , SetupScriptOptions
          )
    installedCabalVersion options' _ _
      | packageName pkg == mkPackageName "Cabal"
          && bt == Custom =
          return (packageVersion pkg, Nothing, options')
    installedCabalVersion options' compiler progdb = do
      index <- maybeGetInstalledPackages options' compiler progdb
      let cabalDepName = mkPackageName "Cabal"
          cabalDepVersion = useCabalVersion options'
          options'' = options'{usePackageIndex = Just index}
      case PackageIndex.lookupDependency index cabalDepName cabalDepVersion of
        [] ->
          dieWithException verbosity $ InstalledCabalVersion (packageName pkg) (useCabalVersion options)
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
      :: SetupScriptOptions
      -> IO (Compiler, ProgramDb, SetupScriptOptions)
    configureCompiler options' = do
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
      index <- maybeGetInstalledPackages options' comp progdb
      return
        ( comp
        , progdb
        , options'
            { useCompiler = Just comp
            , usePackageIndex = Just index
            , useProgramDb = progdb
            }
        )

    -- \| Path to the setup exe cache directory and path to the cached setup
    -- executable.
    cachedSetupDirAndProg
      :: SetupScriptOptions
      -> Version
      -> IO (FilePath, FilePath)
    cachedSetupDirAndProg options' cabalLibVersion = do
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

    -- \| Look up the setup executable in the cache; update the cache if the setup
    -- executable is not found.
    getCachedSetupExecutable
      :: SetupScriptOptions
      -> Version
      -> Maybe InstalledPackageId
      -> IO FilePath
    getCachedSetupExecutable
      options'
      cabalLibVersion
      maybeCabalLibInstalledPkgId = do
        (setupCacheDir, cachedSetupProgFile) <-
          cachedSetupDirAndProg options' cabalLibVersion
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
                  compileSetupExecutable
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
                          (useExtraPathEnv options)
                          (useExtraEnvOverrides options)
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

    -- \| If the Setup.hs is out of date wrt the executable then recompile it.
    -- Currently this is GHC/GHCJS only. It should really be generalised.
    compileSetupExecutable
      :: SetupScriptOptions
      -> Version
      -> Maybe ComponentId
      -> Bool
      -> IO FilePath
    compileSetupExecutable
      options'
      cabalLibVersion
      maybeCabalLibInstalledPkgId
      forceCompile = do
        setupHsNewer <- i setupHs `moreRecentFile` i setupProgFile
        cabalVersionNewer <- i setupVersionFile `moreRecentFile` i setupProgFile
        let outOfDate = setupHsNewer || cabalVersionNewer
        when (outOfDate || forceCompile) $ do
          debug verbosity "Setup executable needs to be updated, compiling..."
          (compiler, progdb, options'') <- configureCompiler options'
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
              cppMacrosFile = setupDir Cabal.Path.</> makeRelativePathEx "setup_macros.h"
              ghcOptions =
                mempty
                  { -- Respect -v0, but don't crank up verbosity on GHC if
                    -- Cabal verbosity is requested. For that, use
                    -- --ghc-option=-v instead!
                    ghcOptVerbosity = Flag (min verbosity normal)
                  , ghcOptMode = Flag GhcModeMake
                  , ghcOptInputFiles = toNubListR [setupHs]
                  , ghcOptOutputFile = Flag $ setupProgFile
                  , ghcOptObjDir = Flag $ setupDir
                  , ghcOptHiDir = Flag $ setupDir
                  , ghcOptSourcePathClear = Flag True
                  , ghcOptSourcePath = case bt of
                      Custom -> toNubListR [sameDirectory]
                      Hooks -> toNubListR [sameDirectory]
                      _ -> mempty
                  , ghcOptPackageDBs = usePackageDB options''
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
                  }
          let ghcCmdLine = renderGhcOptions compiler platform ghcOptions
          when (useVersionMacros options') $
            rewriteFileEx verbosity (i cppMacrosFile) $
              generatePackageVersionMacros (pkgVersion $ package pkg) (map snd selectedDeps)
          case useLoggingHandle options of
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
        return $ i setupProgFile

isCabalPkgId :: PackageIdentifier -> Bool
isCabalPkgId (PackageIdentifier pname _) = pname == mkPackageName "Cabal"
