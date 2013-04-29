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
    sandboxAddSourceSnapshot,
    sandboxDeleteSource,
    sandboxListSources,
    sandboxHcPkg,
    dumpPackageEnvironment,
    withSandboxBinDirOnSearchPath,

    UseSandbox(..), isUseSandbox, whenUsingSandbox,
    ForceGlobalInstall(UseDefaultPackageDBStack), maybeForceGlobalInstall,
    loadConfigOrSandboxConfig,
    initPackageDBIfNeeded,
    maybeWithSandboxDirOnSearchPath,

    WereDepsReinstalled(..),
    reinstallAddSourceDeps,
    maybeReinstallAddSourceDeps,
    maybeUpdateSandboxConfig,

    tryGetIndexFilePath,

    -- FIXME: move somewhere else
    configPackageDB', configCompilerAux'
  ) where

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), GlobalFlags(..), InstallFlags(..)
  , defaultConfigExFlags, defaultInstallFlags, defaultSandboxLocation
  , globalRepos )
import Distribution.Client.Sandbox.Timestamp  ( maybeAddCompilerTimestampRecord
                                              , withAddTimestamps
                                              , withRemoveTimestamps
                                              , withModifiedDeps )
import Distribution.Client.Config             ( SavedConfig(..), loadConfig )
import Distribution.Client.Dependency         ( foldProgress )
import Distribution.Client.Install            ( InstallArgs,
                                                makeInstallContext,
                                                makeInstallPlan,
                                                processInstallPlan,
                                                pruneInstallPlan )
import Distribution.Client.Sandbox.PackageEnvironment
  ( PackageEnvironment(..), IncludeComments(..), PackageEnvironmentType(..)
  , createPackageEnvironment, classifyPackageEnvironment
  , tryLoadSandboxPackageEnvironment, loadUserConfig
  , commentPackageEnvironment, showPackageEnvironmentWithComments
  , sandboxPackageEnvironmentFile, updatePackageEnvironment )
import Distribution.Client.Targets            ( UserTarget(..)
                                              , readUserTargets
                                              , resolveUserTargets )
import Distribution.Client.Types              ( SourcePackageDb(..) )
import Distribution.Client.Utils              ( inDir, tryCanonicalizePath )
import Distribution.PackageDescription.Configuration
                                              ( flattenPackageDescription )
import Distribution.PackageDescription.Parse  ( readPackageDescription )
import Distribution.Simple.Compiler           ( Compiler(..), PackageDB(..)
                                              , PackageDBStack )
import Distribution.Simple.Configure          ( configCompilerAux
                                              , interpretPackageDbFlags )
import Distribution.Simple.PreProcess         ( knownSuffixHandlers )
import Distribution.Simple.Program            ( ProgramConfiguration )
import Distribution.Simple.Setup              ( Flag(..)
                                              , fromFlag, fromFlagOrDefault )
import Distribution.Simple.SrcDist            ( prepareTree )
import Distribution.Simple.Utils              ( die, debug, notice, info
                                              , debugNoWrap, defaultPackageDesc
                                              , intercalate
                                              , createDirectoryIfMissingVerbose )
import Distribution.Package                   ( Package(..) )
import Distribution.System                    ( Platform )
import Distribution.Text                      ( display )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import Distribution.Compat.Env                ( lookupEnv, setEnv )
import qualified Distribution.Client.Sandbox.Index as Index
import qualified Distribution.Simple.Register      as Register
import Control.Exception                      ( assert, bracket_ )
import Control.Monad                          ( forM, unless, when )
import Data.IORef                             ( newIORef, writeIORef, readIORef )
import Data.List                              ( (\\), delete )
import Data.Monoid                            ( mempty, mappend )
import System.Directory                       ( createDirectory
                                              , doesDirectoryExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile
                                              , renameDirectory )
import System.FilePath                        ( (</>), getSearchPath
                                              , searchPathSeparator )


--
-- * Constants
--

-- | The name of the sandbox subdirectory where we keep snapshots of add-source
-- dependencies.
snapshotDirectoryName :: FilePath
snapshotDirectoryName = "snapshots"

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
  (sandboxDir, pkgEnv) <- tryLoadSandboxPackageEnvironment verbosity pkgEnvDir
                          configFileFlag
  dirExists            <- doesDirectoryExist sandboxDir
  -- TODO: Also check for an initialised package DB?
  unless dirExists $
    die ("No sandbox exists at " ++ sandboxDir)
  info verbosity $ "Using a sandbox located at " ++ sandboxDir
  return (sandboxDir, pkgEnv)

-- | Return the name of the package index file for this package environment.
tryGetIndexFilePath :: SavedConfig -> IO FilePath
tryGetIndexFilePath config = do
  let paths = globalLocalRepos . savedGlobalFlags $ config
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
  sandboxDir <- tryCanonicalizePath sandboxDir'
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir

  -- Determine which compiler to use (using the value from ~/.cabal/config).
  userConfig <- loadConfig verbosity (globalConfigFile globalFlags) NoFlag
  (comp, platform, _) <- configCompilerAux (savedConfigureFlags userConfig)

  -- Create the package environment file.
  pkgEnvDir   <- getCurrentDirectory
  createPackageEnvironment verbosity sandboxDir pkgEnvDir
    NoComments comp platform
  (_, pkgEnv) <- tryLoadSandboxPackageEnvironment verbosity pkgEnvDir
                 (globalConfigFile globalFlags)

  -- Create the index file if it doesn't exist.
  indexFile <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)
  Index.createEmpty verbosity indexFile

  -- We don't create the package DB for the default compiler here: it's created
  -- by demand in 'install' and 'configure'. This way, if you run 'sandbox init'
  -- and then 'configure -w /path/to/nondefault-ghc', you'll end up with a
  -- package DB for only one compiler instead of two.

-- | Entry point for the 'cabal sandbox-delete' command.
sandboxDelete :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
sandboxDelete verbosity _sandboxFlags globalFlags = do
  (useSandbox, _) <- loadConfigOrSandboxConfig verbosity
                     (globalConfigFile globalFlags) mempty
  case useSandbox of
    NoSandbox -> die "Not in a sandbox."
    UseSandbox sandboxDir -> do
      pkgEnvDir  <- getCurrentDirectory

      -- Remove the cabal.sandbox.config file
      removeFile (pkgEnvDir </> sandboxPackageEnvironmentFile)

      -- Remove the sandbox directory, unless we're using a shared sandbox.
      let isNonDefaultLocation = sandboxDir /=
                                 (pkgEnvDir </> defaultSandboxLocation)

      when isNonDefaultLocation $
        die $ "Non-default sandbox location used: '" ++ sandboxDir
        ++ "'\nAssuming a shared sandbox. Please delete '"
        ++ sandboxDir ++ "' manually."

      notice verbosity $ "Deleting the sandbox located at " ++ sandboxDir
      removeDirectoryRecursive sandboxDir

-- Common implementation of 'sandboxAddSource' and 'sandboxAddSourceSnapshot'.
doAddSource :: Verbosity -> [FilePath] -> FilePath -> PackageEnvironment -> IO ()
doAddSource verbosity buildTreeRefs sandboxDir pkgEnv = do
  let savedConfig       = pkgEnvSavedConfig pkgEnv
  indexFile            <- tryGetIndexFilePath savedConfig

  -- If we're running 'sandbox add-source' for the first time for this compiler,
  -- we need to create an initial timestamp record.
  (comp, platform, _) <- configCompilerAux . savedConfigureFlags $ savedConfig
  maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
    (compilerId comp) platform

  withAddTimestamps sandboxDir $ do
    -- FIXME: path canonicalisation is done in addBuildTreeRefs, but we do it
    -- twice because of the timestamps file.
    buildTreeRefs' <- mapM tryCanonicalizePath buildTreeRefs
    Index.addBuildTreeRefs verbosity indexFile buildTreeRefs'
    return buildTreeRefs'

-- | Entry point for the 'cabal sandbox add-source' command.
sandboxAddSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                    -> IO ()
sandboxAddSource verbosity buildTreeRefs sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)

  if fromFlagOrDefault False (sandboxSnapshot sandboxFlags)
    then sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv
    else doAddSource verbosity buildTreeRefs sandboxDir pkgEnv

-- | Entry point for the 'cabal sandbox add-source --snapshot' command.
sandboxAddSourceSnapshot :: Verbosity -> [FilePath] -> FilePath
                            -> PackageEnvironment
                            -> IO ()
sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv = do
  let snapshotDir = sandboxDir </> snapshotDirectoryName

  -- Use 'D.S.SrcDist.prepareTree' to copy each package's files to our private
  -- location.
  createDirectoryIfMissingVerbose verbosity True snapshotDir

  -- Collect the package descriptions first, so that if some path does not refer
  -- to a cabal package, we fail immediately.
  pkgs      <- forM buildTreeRefs $ \buildTreeRef ->
    inDir (Just buildTreeRef) $
    return . flattenPackageDescription
            =<< readPackageDescription verbosity
            =<< defaultPackageDesc     verbosity

  -- Copy the package sources to "snapshots/$PKGNAME-$VERSION-tmp". If
  -- 'prepareTree' throws an error at any point, the old snapshots will still be
  -- in consistent state.
  tmpDirs <- forM (zip buildTreeRefs pkgs) $ \(buildTreeRef, pkg) ->
    inDir (Just buildTreeRef) $ do
      let targetDir    = snapshotDir </> (display . packageId $ pkg)
          targetTmpDir = targetDir ++ "-tmp"
      dirExists <- doesDirectoryExist targetTmpDir
      when dirExists $
        removeDirectoryRecursive targetDir
      createDirectory targetTmpDir
      prepareTree verbosity pkg Nothing buildTreeRef targetTmpDir
        knownSuffixHandlers
      return (targetTmpDir, targetDir)

  -- Now rename the "snapshots/$PKGNAME-$VERSION-tmp" dirs to
  -- "snapshots/$PKGNAME-$VERSION".
  snapshots <- forM tmpDirs $ \(targetTmpDir, targetDir) -> do
    dirExists <- doesDirectoryExist targetDir
    when dirExists $
      removeDirectoryRecursive targetDir
    renameDirectory targetTmpDir targetDir
    return targetDir

  -- Once the packages are copied, just 'add-source' them as usual.
  doAddSource verbosity snapshots sandboxDir pkgEnv

-- | Entry point for the 'cabal sandbox delete-source' command.
sandboxDeleteSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                       -> IO ()
sandboxDeleteSource verbosity buildTreeRefs _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  indexFile            <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)

  withRemoveTimestamps sandboxDir $ do
    -- FIXME: path canonicalisation is done in addBuildTreeRefs, but we do it
    -- twice because of the timestamps file.
    buildTreeRefs' <- mapM tryCanonicalizePath buildTreeRefs
    Index.removeBuildTreeRefs verbosity indexFile buildTreeRefs'
    return buildTreeRefs'

-- | Entry point for the 'cabal sandbox list-sources' command.
sandboxListSources :: Verbosity -> SandboxFlags -> GlobalFlags
                      -> IO ()
sandboxListSources verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                           (globalConfigFile globalFlags)
  indexFile            <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)

  refs <- Index.listBuildTreeRefs verbosity Index.ListIgnored indexFile
  when (null refs) $
    notice verbosity $ "Index file '" ++ indexFile
    ++ "' has no references to local build trees."
  when (not . null $ refs) $ do
    notice verbosity $ "Source dependencies registered "
      ++ "in the current sandbox ('" ++ sandboxDir ++ "'):\n\n"
    mapM_ putStrLn refs
    notice verbosity $ "\nTo unregister source dependencies, "
                       ++ "use the 'sandbox delete-source' command."

-- | Invoke the @hc-pkg@ tool with provided arguments, restricted to the
-- sandbox.
sandboxHcPkg :: Verbosity -> SandboxFlags -> GlobalFlags -> [String] -> IO ()
sandboxHcPkg verbosity _sandboxFlags globalFlags extraArgs = do
  (_sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                           (globalConfigFile globalFlags)
  let configFlags = savedConfigureFlags . pkgEnvSavedConfig $ pkgEnv
      dbStack     = configPackageDB' configFlags ForceGlobalInstall
  (comp, _platform, conf) <- configCompilerAux' configFlags

  Register.invokeHcPkg verbosity comp conf dbStack extraArgs

--
-- * Helpers for writing code that works both inside and outside a sandbox.
--

-- | Are we using a sandbox?
data UseSandbox = UseSandbox FilePath | NoSandbox

-- | Convert a @UseSandbox@ value to a boolean. Useful in conjunction with
-- @when@.
isUseSandbox :: UseSandbox -> Bool
isUseSandbox (UseSandbox _) = True
isUseSandbox NoSandbox      = False

-- | Execute an action only if we're in a sandbox, feeding to it the path to the
-- sandbox directory.
whenUsingSandbox :: UseSandbox -> (FilePath -> IO ()) -> IO ()
whenUsingSandbox NoSandbox               _   = return ()
whenUsingSandbox (UseSandbox sandboxDir) act = act sandboxDir

-- | Check which type of package environment we're in and return a
-- correctly-initialised @SavedConfig@ and a @UseSandbox@ value that indicates
-- whether we're working in a sandbox.
loadConfigOrSandboxConfig :: Verbosity
                             -> Flag FilePath -- ^ --config-file
                             -> Flag Bool     -- ^ Ignored if we're in a sandbox.
                             -> IO (UseSandbox, SavedConfig)
loadConfigOrSandboxConfig verbosity configFileFlag userInstallFlag = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    SandboxPackageEnvironment -> do
      (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity configFileFlag
                              -- ^ Prints an error message and exits on error.
      let config = pkgEnvSavedConfig pkgEnv
      return (UseSandbox sandboxDir, config)

    UserPackageEnvironment    -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      userConfig <- loadUserConfig verbosity currentDir
      return (NoSandbox, config `mappend` userConfig)

    AmbientPackageEnvironment      -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      return (NoSandbox, config)

-- | If we're in a sandbox, call @withSandboxBinDirOnSearchPath@, otherwise do
-- nothing.
maybeWithSandboxDirOnSearchPath :: UseSandbox -> IO a -> IO a
maybeWithSandboxDirOnSearchPath NoSandbox               act = act
maybeWithSandboxDirOnSearchPath (UseSandbox sandboxDir) act =
  withSandboxBinDirOnSearchPath sandboxDir $ act

-- | Had reinstallAddSourceDeps actually reinstalled any dependencies?
data WereDepsReinstalled = ReinstalledSomeDeps | NoDepsReinstalled

-- | Reinstall those add-source dependencies that have been modified since
-- we've last installed them.
reinstallAddSourceDeps :: Verbosity -> SavedConfig -> Flag (Maybe Int)
                          -> FilePath -> GlobalFlags
                          -> IO WereDepsReinstalled
reinstallAddSourceDeps verbosity config numJobsFlag sandboxDir globalFlags = do
  let configFlags    = savedConfigureFlags   config
      configExFlags  = defaultConfigExFlags         `mappend`
                       savedConfigureExFlags config
      installFlags'  = defaultInstallFlags          `mappend`
                       savedInstallFlags     config
      installFlags   = installFlags' {
        installNumJobs = installNumJobs installFlags' `mappend` numJobsFlag
        }
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags

  indexFile            <- tryGetIndexFilePath config
  buildTreeRefs        <- Index.listBuildTreeRefs verbosity
                          Index.DontListIgnored indexFile
  retVal               <- newIORef NoDepsReinstalled

  unless (null buildTreeRefs) $ do
    (comp, platform, conf) <- configCompilerAux' configFlags
    let compId              = compilerId comp

    withModifiedDeps verbosity sandboxDir compId platform $ \modifiedDeps -> do
      assert (null $ modifiedDeps \\ buildTreeRefs) (return ())
      unless (null modifiedDeps) $ do
        let targetNames    = (".":modifiedDeps)
            targetsToPrune = [UserTargetLocalDir "."]

        notice verbosity "Installing add-source dependencies..."
        targets <- readUserTargets verbosity targetNames

        let args :: InstallArgs
            args = ((configPackageDB' configFlags ForceGlobalInstall)
                   ,(globalRepos globalFlags')
                   ,comp, platform, conf
                   ,globalFlags', configFlags, configExFlags, installFlags
                   ,mempty)

            logMsg message rest = debugNoWrap verbosity message >> rest

        -- Using the low-level install interface instead of the high-level
        -- 'install' action allows us to make changes to the install plan before
        -- processing it. Here we need to prune the "." target from the install
        -- plan. The same mechanism is used to implement 'install
        -- --only-dependencies'.
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
          writeIORef retVal ReinstalledSomeDeps

  readIORef retVal

-- | Check if a sandbox is present and call @reinstallAddSourceDeps@ in that
-- case.
maybeReinstallAddSourceDeps :: Verbosity -> Flag (Maybe Int) -> GlobalFlags
                             -> IO (UseSandbox, WereDepsReinstalled)
maybeReinstallAddSourceDeps verbosity numJobsFlag globalFlags = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    AmbientPackageEnvironment -> return (NoSandbox, NoDepsReinstalled)
    UserPackageEnvironment    -> return (NoSandbox, NoDepsReinstalled)
    SandboxPackageEnvironment -> do
      (useSandbox, config) <- loadConfigOrSandboxConfig verbosity
                              (globalConfigFile globalFlags) mempty
      let sandboxDir = case useSandbox of
            UseSandbox d -> d;
            _            -> error "Distribution.Client.Sandbox.\
                                  \maybeInstallAddSourceDeps: can't happen"
      depsReinstalled <- reinstallAddSourceDeps verbosity config
                                   numJobsFlag sandboxDir globalFlags
      return (useSandbox, depsReinstalled)

-- | Update the 'with-compiler' and 'package-db' fields in the auto-generated
-- sandbox config file if the user has configured the project with a different
-- compiler. Note that we don't auto-enable things like 'library-profiling' (for
-- now?) even if the user has passed '--enable-library-profiling' to
-- 'configure'. These options are supposed to be set in cabal.config.
maybeUpdateSandboxConfig :: Verbosity
                            -> SavedConfig -- ^ old config
                            -> ConfigFlags -- ^ new configure flags
                            -> IO ()
maybeUpdateSandboxConfig verbosity savedConfig newConfigFlags = do
  let oldConfigFlags = savedConfigureFlags savedConfig

      oldHcFlavor    = configHcFlavor   oldConfigFlags
      oldHcPath      = configHcPath     oldConfigFlags
      oldPackageDBs  = configPackageDBs oldConfigFlags

      newHcFlavor    = configHcFlavor   newConfigFlags
      newHcPath      = configHcPath     newConfigFlags
      newPackageDBs  = configPackageDBs newConfigFlags

  when ((oldHcFlavor /= newHcFlavor)
        || (oldHcPath /= newHcPath)
        || (oldPackageDBs /= newPackageDBs)) $ do
    pkgEnvDir <- getCurrentDirectory
    updatePackageEnvironment verbosity pkgEnvDir
      newHcFlavor newHcPath newPackageDBs

--
-- Utils (transitionary)
--
-- FIXME: configPackageDB' and configCompilerAux' don't really belong in this
-- module
--

-- | Force the usage of the global package DB even though @configUserInstall@
-- may be @True@.
--
-- We use @userInstallDirs@ in sandbox mode to prevent @cabal-install@ from
-- doing unnecessary things like invoking itself via @sudo@ (see commit
-- 7b2e3630f2ada8a56bf9100144e1bb9acbe6dc6a and 'rootCmd' in
-- "Distribution.Client.Install"), but in this particular case we want
-- @configUserInstall@ to be @False@ to prevent @UserPackageDB@ from being added
-- to the package DB stack (see #1183 and @interpretPackageDbFlags@ in
-- "Distribution.Simple.Configure").
--
-- In the future we may want to distinguish between global, user and sandbox
-- install types.
data ForceGlobalInstall = ForceGlobalInstall
                        | UseDefaultPackageDBStack

-- | If we're in a sandbox, add only the global package db to the package db
-- stack, otherwise use the default behaviour.
maybeForceGlobalInstall :: UseSandbox -> ForceGlobalInstall
maybeForceGlobalInstall NoSandbox      = UseDefaultPackageDBStack
maybeForceGlobalInstall (UseSandbox _) = ForceGlobalInstall

configPackageDB' :: ConfigFlags -> ForceGlobalInstall -> PackageDBStack
configPackageDB' cfg force =
    interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = case force of
      ForceGlobalInstall       -> False
      UseDefaultPackageDBStack -> fromFlagOrDefault True (configUserInstall cfg)

configCompilerAux' :: ConfigFlags
                   -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerAux' configFlags =
  configCompilerAux configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
