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
    sandboxConfigure,
    sandboxBuild,
    sandboxInstall,

    dumpPackageEnvironment,
    withSandboxBinDirOnSearchPath
  ) where

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), ConfigExFlags(..), GlobalFlags(..)
  , InstallFlags(..), globalRepos
  , defaultInstallFlags, defaultConfigExFlags, defaultSandboxLocation
  , installCommand )
import Distribution.Client.Config             ( SavedConfig(..), loadConfig )
import Distribution.Client.Configure          ( configure )
import Distribution.Client.Install            ( makeInstallContext
                                              , makeInstallPlan
                                              , processInstallPlan
                                              , pruneInstallPlan
                                              , InstallArgs )
import Distribution.Client.PackageEnvironment
  ( PackageEnvironment(..), IncludeComments(..)
  , createPackageEnvironment, tryLoadPackageEnvironment
  , commentPackageEnvironment
  , showPackageEnvironmentWithComments
  , setPackageDB
  , sandboxPackageEnvironmentFile )
import Distribution.Client.SetupWrapper
  ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Targets            ( readUserTargets
                                              , resolveUserTargets
                                              , UserTarget(..) )
import Distribution.Client.Types              ( SourcePackageDb(packageIndex) )
import Distribution.Client.Dependency.Types   ( foldProgress )
import Distribution.Simple.Compiler           ( Compiler
                                              , PackageDB(..), PackageDBStack )
import Distribution.Simple.Configure          ( configCompilerAux
                                              , interpretPackageDbFlags )
import Distribution.Simple.Program            ( ProgramConfiguration
                                              , defaultProgramConfiguration )
import Distribution.Simple.Setup              ( Flag(..), toFlag, fromFlag
                                              , BuildFlags(..), HaddockFlags(..)
                                              , buildCommand, fromFlagOrDefault )
import Distribution.Simple.Utils              ( die, debug, notice, info
                                              , debugNoWrap, intercalate
                                              , createDirectoryIfMissingVerbose )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import Distribution.Compat.Env                ( lookupEnv, setEnv )
import Distribution.System                    ( Platform )
import qualified Distribution.Client.Index as Index
import qualified Distribution.Simple.Register as Register
import Control.Exception                      ( bracket_ )
import Control.Monad                          ( unless, when )
import Data.Monoid                            ( mappend, mempty )
import Data.List                              ( delete )
import System.Directory                       ( canonicalizePath
                                              , doesDirectoryExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile )
import System.FilePath                        ( (</>), getSearchPath
                                              , searchPathSeparator )


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

-- | Entry point for the 'cabal sandbox-configure' command.
sandboxConfigure :: Verbosity -> SandboxFlags -> ConfigFlags -> ConfigExFlags
                    -> [String] -> GlobalFlags -> IO ()
sandboxConfigure verbosity
  _sandboxFlags configFlags configExFlags extraArgs globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)

  let config         = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = savedConfigureExFlags config `mappend` configExFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux configFlags'

  -- If the user has set the -w option, we may need to create the package DB for
  -- this compiler.
  let configFlags''  = setPackageDB sandboxDir comp configFlags'
  initPackageDBIfNeeded verbosity configFlags'' comp conf

  withSandboxBinDirOnSearchPath sandboxDir $
    configure verbosity
              (configPackageDB' configFlags'') (globalRepos globalFlags')
              comp platform conf configFlags'' configExFlags' extraArgs

-- | Entry point for the 'cabal sandbox-build' command.
sandboxBuild :: Verbosity -> SandboxFlags -> BuildFlags -> GlobalFlags
                -> [String] -> IO ()
sandboxBuild verbosity sandboxFlags buildFlags' globalFlags extraArgs = do
  let setupScriptOptions = defaultSetupScriptOptions {
        useDistPref = fromFlagOrDefault
                      (useDistPref defaultSetupScriptOptions)
                      (buildDistPref buildFlags)
        }
      buildFlags = buildFlags' {
        buildVerbosity = toFlag verbosity
        }
  -- Check that the sandbox exists.
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  indexFile            <- tryGetIndexFilePath pkgEnv
  buildTreeRefs        <- Index.listBuildTreeRefs verbosity indexFile

  -- Install all add-source dependencies of the current package into the
  -- sandbox.
  unless (null buildTreeRefs) $
    sandboxInstall verbosity sandboxFlags mempty mempty mempty mempty
      (".":buildTreeRefs) mempty [UserTargetLocalDir "."]

  -- Actually build the package.
  -- TODO: Do the "you should run configure before build" check before installing
  -- add-source dependencies.
  withSandboxBinDirOnSearchPath sandboxDir $
    setupWrapper verbosity setupScriptOptions Nothing
      (buildCommand defaultProgramConfiguration) (const buildFlags) extraArgs

-- | Entry point for the 'cabal sandbox-install' command.
sandboxInstall :: Verbosity -> SandboxFlags -> ConfigFlags -> ConfigExFlags
                  -> InstallFlags -> HaddockFlags
                  -> [String] -> GlobalFlags
                  -> [UserTarget] -- ^ Targets to prune from the install plan.
                  -> IO ()
sandboxInstall verbosity _sandboxFlags _configFlags _configExFlags
  installFlags _haddockFlags _extraArgs _globalFlags _targetsToPrune
  | fromFlagOrDefault False (installOnly installFlags)
  -- TODO: It'd be nice if this picked up the -w flag passed to
  -- sandbox-configure.  Right now, running
  --
  -- $ cabal sandbox-init && cabal sandbox-configure -w /path/to/ghc
  --   && cabal sandbox-build && cabal sandbox-install
  --
  -- performs the compilation twice unless you also pass -w to sandbox-install.
  = setupWrapper verbosity defaultSetupScriptOptions Nothing
    installCommand (const mempty) []

sandboxInstall verbosity _sandboxFlags configFlags configExFlags
  installFlags haddockFlags extraArgs globalFlags
  targetsToPrune = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  targets              <- readUserTargets verbosity extraArgs

  let config         = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = defaultConfigExFlags         `mappend`
                       savedConfigureExFlags config `mappend` configExFlags
      installFlags'  = defaultInstallFlags          `mappend`
                       savedInstallFlags     config `mappend` installFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, platform, conf) <- configCompilerAux' configFlags'

  -- If the user has set the -w option, we may need to create the package DB for
  -- this compiler.
  let configFlags''  = setPackageDB sandboxDir comp configFlags'

      args :: InstallArgs
      args = ((configPackageDB' configFlags''), (globalRepos globalFlags'),
              comp, platform, conf,
              globalFlags', configFlags'', configExFlags', installFlags',
              haddockFlags)

      logMsg message rest = debugNoWrap verbosity message >> rest

  initPackageDBIfNeeded verbosity configFlags'' comp conf

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

configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
  -- The userInstall parameter is set to False so that interpretPackageDbFlags
  -- doesn't add UserPackageDb to the PackageDbStack (see #1183).
  -- FIXME: This is a bit fragile, maybe change the boolean parameter to
  -- UserInstall | GlobalInstall | UseSandbox ?
  interpretPackageDbFlags {- userInstall = -} False (configPackageDBs cfg)

configCompilerAux' :: ConfigFlags
                      -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerAux' configFlags =
  configCompilerAux configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
