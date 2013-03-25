-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- UI for the sandboxing functionality.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox (
    sandboxInit,
    sandboxDelete,
    sandboxAddSource,

    dumpPackageEnvironment,
    withSandboxBinDirOnSearchPath,

    MaybePkgEnv(..),
    toSavedConfig, loadConfigOrPkgEnv, maybeLoadPackageEnvironment,
    maybeSetPackageDB,
    maybeInitPackageDBIfNeeded,
    maybeWithSandboxDirOnSearchPath,
    maybeInstallAddSourceDeps,
  ) where

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), GlobalFlags(..)
  , defaultConfigExFlags, defaultInstallFlags, defaultSandboxLocation
  , globalRepos )
import Distribution.Client.Config             ( SavedConfig(..), loadConfig )
import Distribution.Client.Dependency         ( foldProgress )
import Distribution.Client.Install            ( InstallArgs,
                                                makeInstallContext,
                                                makeInstallPlan,
                                                processInstallPlan,
                                                pruneInstallPlan )
import Distribution.Client.PackageEnvironment
  ( PackageEnvironment(..), IncludeComments(..), PackageEnvironmentType(..)
  , createPackageEnvironment, classifyPackageEnvironment
  , tryLoadPackageEnvironment, loadUserConfig, setPackageDB
  , commentPackageEnvironment, showPackageEnvironmentWithComments
  , sandboxPackageEnvironmentFile )
import Distribution.Client.Targets            ( UserTarget(..)
                                              , readUserTargets
                                              , resolveUserTargets )
import Distribution.Client.Types              ( SourcePackageDb(..) )
import Distribution.Simple.Compiler           ( Compiler, PackageDB(..) )
import Distribution.Simple.Configure          ( configCompilerAux
                                              , interpretPackageDbFlags )
import Distribution.Simple.Program            ( ProgramConfiguration )
import Distribution.Simple.Setup              ( Flag(..)
                                              , fromFlag, fromFlagOrDefault )
import Distribution.Simple.Utils              ( die, debug, notice, info
                                              , debugNoWrap
                                              , intercalate
                                              , createDirectoryIfMissingVerbose )
import Distribution.System                    ( Platform )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import Distribution.Compat.Env                ( lookupEnv, setEnv )
import qualified Distribution.Client.Index as Index
import qualified Distribution.Simple.Register as Register
import Control.Exception                      ( bracket_ )
import Control.Monad                          ( unless, when )
import Data.List                              ( delete )
import Data.Monoid                            ( mempty, mappend )
import System.Directory                       ( canonicalizePath
                                              , doesDirectoryExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile )
import System.FilePath                        ( (</>), getSearchPath
                                              , searchPathSeparator )


--
-- * Basic sandbox functions.
--

-- | Load the default package environment file. In addition to a
-- @PackageEnvironment@, also return a canonical path to the sandbox. Exit with
-- error if the sandbox directory or the package environment file do not exist.
tryLoadSandboxConfig :: Verbosity -> Flag FilePath
                        -> IO (FilePath, PackageEnvironment)
tryLoadSandboxConfig verbosity configFileFlag = do
  pkgEnvDir            <- getCurrentDirectory
  (sandboxDir, pkgEnv) <- tryLoadPackageEnvironment verbosity pkgEnvDir
                          configFileFlag
  dirExists            <- doesDirectoryExist sandboxDir
  -- TODO: Also check for an initialised package DB?
  unless dirExists $
    die ("No sandbox exists at " ++ sandboxDir)
  info verbosity $ "Using a sandbox located at " ++ sandboxDir
  return (sandboxDir, pkgEnv)

-- | Return the name of the package index file for this package environment.
tryGetIndexFilePath :: PackageEnvironment -> IO FilePath
tryGetIndexFilePath pkgEnv = do
  let paths = globalLocalRepos . savedGlobalFlags . pkgEnvSavedConfig $ pkgEnv
  case paths of
    []  -> die $ "Distribution.Client.Sandbox.tryGetIndexFilePath: " ++
           "no local repos found"
    [p] -> return $ p </> Index.defaultIndexFileName
    _   -> die $ "Distribution.Client.Sandbox.tryGetIndexFilePath: " ++
           "too many local repos found"

-- | Temporarily add $SANDBOX_DIR/bin to $PATH.
withSandboxBinDirOnSearchPath :: FilePath -> IO a -> IO a
withSandboxBinDirOnSearchPath sandboxDir = bracket_ addBinDir rmBinDir
  where
    -- TODO: Instead of modifying the global process state, it'd be better to
    -- set the environment individually for each subprocess invocation. This
    -- will have to wait until the Shell monad is implemented; without it the
    -- required changes are too intrusive.
    addBinDir :: IO ()
    addBinDir = do
      mbOldPath <- lookupEnv "PATH"
      let newPath = maybe sandboxBin ((++) sandboxBin . (:) searchPathSeparator)
                    mbOldPath
      setEnv "PATH" newPath

    rmBinDir :: IO ()
    rmBinDir = do
      oldPath <- getSearchPath
      let newPath = intercalate [searchPathSeparator]
                    (delete sandboxBin oldPath)
      setEnv "PATH" newPath

    sandboxBin = sandboxDir </> "bin"

-- | Initialise a package DB for this compiler if it doesn't exist.
initPackageDBIfNeeded :: Verbosity -> ConfigFlags
                         -> Compiler -> ProgramConfiguration
                         -> IO ()
initPackageDBIfNeeded verbosity configFlags comp conf = do
  -- TODO: Is pattern-matching here really safe?
  let [Just (SpecificPackageDB dbPath)] = configPackageDBs configFlags
  packageDBExists <- doesDirectoryExist dbPath
  unless packageDBExists $
    Register.initPackageDB verbosity comp conf dbPath
  when packageDBExists $
    debug verbosity $ "The package database already exists: " ++ dbPath

-- | Entry point for the 'cabal dump-pkgenv' command.
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
dumpPackageEnvironment verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  commentPkgEnv        <- commentPackageEnvironment sandboxDir
  putStrLn . showPackageEnvironmentWithComments (Just commentPkgEnv) $ pkgEnv

-- | Entry point for the 'cabal sandbox-init' command.
sandboxInit :: Verbosity -> SandboxFlags  -> GlobalFlags -> IO ()
sandboxInit verbosity sandboxFlags globalFlags = do
  -- Create the sandbox directory.
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  createDirectoryIfMissingVerbose verbosity True sandboxDir'
  sandboxDir <- canonicalizePath sandboxDir'
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir

  -- Determine which compiler to use (using the value from ~/.cabal/config).
  userConfig   <- loadConfig verbosity (globalConfigFile globalFlags) NoFlag
  (comp, _, conf) <- configCompilerAux (savedConfigureFlags userConfig)

  -- Create the package environment file.
  pkgEnvDir   <- getCurrentDirectory
  createPackageEnvironment verbosity sandboxDir pkgEnvDir NoComments comp
  (_, pkgEnv) <- tryLoadPackageEnvironment verbosity pkgEnvDir
                 (globalConfigFile globalFlags)

  -- Create the index file if it doesn't exist.
  indexFile <- tryGetIndexFilePath pkgEnv
  Index.createEmpty verbosity indexFile

  -- Create the package DB for this compiler if it doesn't exist. If the user
  -- later chooses a different compiler with -w, the sandbox for that compiler
  -- will be created on demand.
  initPackageDBIfNeeded verbosity
    (savedConfigureFlags . pkgEnvSavedConfig $ pkgEnv) comp conf

-- | Entry point for the 'cabal sandbox-delete' command.
sandboxDelete :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
sandboxDelete verbosity sandboxFlags _globalFlags = do
  pkgEnvDir <- getCurrentDirectory
  removeFile (pkgEnvDir </> sandboxPackageEnvironmentFile)
  if sandboxLoc == defaultSandboxLocation
    then do
      sandboxDir <- canonicalizePath sandboxLoc
      notice verbosity $ "Deleting the sandbox located at " ++ sandboxDir
      removeDirectoryRecursive sandboxDir
    else
      die $ "Non-default sandbox location used: " ++ sandboxLoc
      ++ "\nPlease delete manually."
  where
    sandboxLoc = fromFlagOrDefault defaultSandboxLocation
                 (sandboxLocation sandboxFlags)

-- | Entry point for the 'cabal sandbox-add-source' command.
sandboxAddSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                    -> IO ()
sandboxAddSource verbosity buildTreeRefs _sandboxFlags globalFlags = do
  (_sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                           (globalConfigFile globalFlags)
  indexFile             <- tryGetIndexFilePath pkgEnv

  Index.addBuildTreeRefs verbosity indexFile buildTreeRefs

--
-- * Helpers for working with @SavedConfig@ and @PackageEnvironment@ uniformly.
--

-- | Helper for working with @SavedConfig@ and @PackageEnvironment@ as a single
-- type.
data MaybePkgEnv = JustConfig SavedConfig
                 | JustPkgEnv PackageEnvironment FilePath
                 | NoPkgEnv

-- | Extract a @SavedConfig@ from @MaybePkgEnv@.
toSavedConfig :: MaybePkgEnv -> SavedConfig
toSavedConfig (JustConfig sc)   = sc
toSavedConfig (JustPkgEnv pe _) = pkgEnvSavedConfig pe
toSavedConfig NoPkgEnv          =
  error $ "Distribution.Client.Sandbox.toSavedConfig: no config given!"

-- | If the current directory contains a 'cabal.sandbox.config', return a
-- @PackageEnvironment@. If it contains just 'cabal.config', load that file and
-- @mappend@ in top of the default config. Otherwise, just call @loadConfig@.
loadConfigOrPkgEnv :: Verbosity -> Flag FilePath -> Flag Bool
                      -> IO MaybePkgEnv
loadConfigOrPkgEnv verbosity configFileFlag userInstallFlag = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    SandboxPackageEnvironment -> do
      (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity configFileFlag
                              -- ^ Prints an error message and exits on error.
      return (JustPkgEnv pkgEnv sandboxDir)

    UserPackageEnvironment    -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      userConfig <- loadUserConfig verbosity currentDir
      return (JustConfig $ config `mappend` userConfig)

    NoPackageEnvironment      -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      return (JustConfig config)

-- | Like @loadConfigOrPkgEnv@, but don't call @loadConfig@ if this is not a
-- sandbox. Saves us from doing unnecessary work.
maybeLoadPackageEnvironment :: Verbosity -> Flag FilePath -> IO MaybePkgEnv
maybeLoadPackageEnvironment verbosity configFileFlag = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    SandboxPackageEnvironment -> do
      (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity configFileFlag
                              -- ^ Prints an error message and exits on error.
      return (JustPkgEnv pkgEnv sandboxDir)

    _                         -> return NoPkgEnv

-- | If we're in a sandbox, call @setPackageDB@, otherwise do nothing.
maybeSetPackageDB :: MaybePkgEnv -> Compiler -> ConfigFlags -> ConfigFlags
maybeSetPackageDB NoPkgEnv                  _    configFlags = configFlags
maybeSetPackageDB (JustConfig _)            _    configFlags = configFlags
maybeSetPackageDB (JustPkgEnv _ sandboxDir) comp configFlags =
  setPackageDB sandboxDir comp configFlags

-- | If we're in a sandbox, call @initPackageDBIfNeeded@, otherwise do nothing.
maybeInitPackageDBIfNeeded :: MaybePkgEnv -> Verbosity -> ConfigFlags
                              -> Compiler -> ProgramConfiguration -> IO ()
maybeInitPackageDBIfNeeded NoPkgEnv       _ _ _ _ = return ()
maybeInitPackageDBIfNeeded (JustConfig _) _ _ _ _ = return ()
maybeInitPackageDBIfNeeded (JustPkgEnv _ _) verbosity configFlags comp conf =
  initPackageDBIfNeeded verbosity configFlags comp conf

-- | If we're in a sandbox, call @withSandboxBinDirOnSearchPath@, otherwise do
-- nothing.
maybeWithSandboxDirOnSearchPath :: MaybePkgEnv -> IO a -> IO a
maybeWithSandboxDirOnSearchPath NoPkgEnv       act = act
maybeWithSandboxDirOnSearchPath (JustConfig _) act = act
maybeWithSandboxDirOnSearchPath (JustPkgEnv _ sandboxDir) act =
  withSandboxBinDirOnSearchPath sandboxDir $ act

-- | If we're in a sandbox, (re)install all add-source dependencies of the
-- current package into the sandbox, otherwise do nothing.
maybeInstallAddSourceDeps :: MaybePkgEnv -> Verbosity -> GlobalFlags -> IO ()
maybeInstallAddSourceDeps NoPkgEnv       _ _ = return ()
maybeInstallAddSourceDeps (JustConfig _) _ _ = return ()
maybeInstallAddSourceDeps
  (JustPkgEnv pkgEnv sandboxDir) verbosity globalFlags = do
  indexFile            <- tryGetIndexFilePath pkgEnv
  buildTreeRefs        <- Index.listBuildTreeRefs verbosity indexFile

  unless (null buildTreeRefs) $ do
    let targetNames    = (".":buildTreeRefs)
        targetsToPrune = [UserTargetLocalDir "."]
        config         = pkgEnvSavedConfig pkgEnv
        configFlags    = savedConfigureFlags   config
        configExFlags  = defaultConfigExFlags         `mappend`
                         savedConfigureExFlags config
        installFlags   = defaultInstallFlags          `mappend`
                         savedInstallFlags     config
        globalFlags'   = savedGlobalFlags      config `mappend` globalFlags

    (comp, platform, conf) <- configCompilerAux' configFlags
    targets <- readUserTargets verbosity targetNames

    let args :: InstallArgs
        args = ((interpretPackageDbFlags {- userInstall = -} False
                 (configPackageDBs configFlags))
               ,(globalRepos globalFlags')
               ,comp, platform, conf
               ,globalFlags', configFlags, configExFlags, installFlags
               ,mempty)

        logMsg message rest = debugNoWrap verbosity message >> rest

    -- Using the low-level install interface instead of the high-level 'install'
    -- action allows us to make changes to the install plan before processing
    -- it. Here we need to prune the "." target from the install plan. The same
    -- mechanism is used to implement 'install --only-dependencies'.
    withSandboxBinDirOnSearchPath sandboxDir $ do
      installContext@(_,sourcePkgDb,_,_) <-
        makeInstallContext verbosity args targets

      toPrune <- resolveUserTargets verbosity
                 (fromFlag $ globalWorldFile globalFlags')
                 (packageIndex sourcePkgDb)
                 targetsToPrune

      installPlan     <- foldProgress logMsg die return =<<
                         (fmap (\p -> p >>= if not . null $ targetsToPrune
                                            then pruneInstallPlan toPrune
                                            else return)
                          $ makeInstallPlan verbosity args installContext)

      processInstallPlan verbosity args installContext installPlan

  where
    -- Copied from Main.hs. FIXME: Remove duplication.
    configCompilerAux' :: ConfigFlags
                          -> IO (Compiler, Platform, ProgramConfiguration)
    configCompilerAux' configFlags =
      configCompilerAux configFlags
      --FIXME: make configCompilerAux use a sensible verbosity
      { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
