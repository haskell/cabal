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

    dumpPackageEnvironment
  ) where

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), ConfigExFlags(..), GlobalFlags(..)
  , InstallFlags(..), globalRepos
  , defaultInstallFlags, defaultConfigExFlags, defaultSandboxLocation
  , installCommand )
import Distribution.Client.Config             ( SavedConfig(..), loadConfig )
import Distribution.Client.Configure          ( configure )
import Distribution.Client.Install            ( install )
import Distribution.Client.PackageEnvironment
  ( PackageEnvironment(..)
  , createPackageEnvironment, tryLoadPackageEnvironment
  , commentPackageEnvironment
  , showPackageEnvironmentWithComments
  , setPackageDB
  , sandboxPackageEnvironmentFile )
import Distribution.Client.SetupWrapper
  ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Targets            ( readUserTargets )
import Distribution.Simple.Compiler           ( Compiler
                                              , PackageDB(..), PackageDBStack )
import Distribution.Simple.Configure          ( configCompilerAux
                                              , interpretPackageDbFlags )
import Distribution.Simple.Program            ( ProgramConfiguration
                                              , defaultProgramConfiguration )
import Distribution.Simple.Setup              ( Flag(..), toFlag
                                              , BuildFlags(..), HaddockFlags(..)
                                              , buildCommand, fromFlagOrDefault )
import Distribution.Simple.Utils              ( die, debug, notice, info
                                              , createDirectoryIfMissingVerbose )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import qualified Distribution.Client.Index as Index
import qualified Distribution.Simple.Register as Register
import Control.Monad                          ( unless, when )
import Data.Monoid                            ( mappend, mempty )
import System.Directory                       ( canonicalizePath
                                              , doesDirectoryExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile )
import System.FilePath                        ( (</>) )


-- | Given a 'SandboxFlags' record, return a canonical path to the
-- sandbox. Exits with error if the sandbox directory does not exist or is not
-- properly initialised.
getSandboxLocation :: Verbosity -> SandboxFlags -> IO FilePath
getSandboxLocation verbosity sandboxFlags = do
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  sandboxDir <- canonicalizePath sandboxDir'
  dirExists  <- doesDirectoryExist sandboxDir
  -- TODO: Also check for an initialised package DB?
  unless dirExists $
    die ("No sandbox exists at " ++ sandboxDir)
  info verbosity $ "Using a sandbox located at " ++ sandboxDir
  return sandboxDir

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
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> IO ()
dumpPackageEnvironment verbosity sandboxFlags = do
  sandboxDir    <- getSandboxLocation verbosity sandboxFlags
  pkgEnvDir     <- getCurrentDirectory
  pkgEnv        <- tryLoadPackageEnvironment verbosity sandboxDir pkgEnvDir
  commentPkgEnv <- commentPackageEnvironment pkgEnvDir
  putStrLn . showPackageEnvironmentWithComments commentPkgEnv $ pkgEnv

-- | Entry point for the 'cabal sandbox-init' command.
sandboxInit :: Verbosity -> SandboxFlags  -> GlobalFlags -> IO ()
sandboxInit verbosity sandboxFlags _globalFlags = do
  -- Create the sandbox directory.
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  createDirectoryIfMissingVerbose verbosity True sandboxDir'
  sandboxDir <- canonicalizePath sandboxDir'
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir

  -- Determine which compiler to use (using the value from ~/.cabal/config).
  userConfig   <- loadConfig verbosity NoFlag NoFlag
  (comp, conf) <- configCompilerAux (savedConfigureFlags userConfig)

  -- Create the package environment file.
  pkgEnvDir <- getCurrentDirectory
  pkgEnv    <- createPackageEnvironment verbosity sandboxDir pkgEnvDir comp

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
sandboxAddSource :: Verbosity -> SandboxFlags -> [FilePath] -> IO ()
sandboxAddSource verbosity sandboxFlags buildTreeRefs = do
  sandboxDir <- getSandboxLocation verbosity sandboxFlags
  pkgEnvDir  <- getCurrentDirectory
  pkgEnv     <- tryLoadPackageEnvironment verbosity sandboxDir pkgEnvDir
  indexFile  <- tryGetIndexFilePath pkgEnv

  Index.addBuildTreeRefs verbosity indexFile buildTreeRefs

-- | Entry point for the 'cabal sandbox-configure' command.
sandboxConfigure :: Verbosity -> SandboxFlags -> ConfigFlags -> ConfigExFlags
                    -> [String] -> GlobalFlags -> IO ()
sandboxConfigure verbosity
  sandboxFlags configFlags configExFlags extraArgs globalFlags = do
  sandboxDir <- getSandboxLocation verbosity sandboxFlags
  pkgEnvDir  <- getCurrentDirectory
  pkgEnv     <- tryLoadPackageEnvironment verbosity sandboxDir pkgEnvDir

  let config         = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = savedConfigureExFlags config `mappend` configExFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, conf) <- configCompilerAux configFlags'

  -- If the user has set the -w option, we may need to create the package DB for
  -- this compiler.
  let configFlags''  = setPackageDB sandboxDir comp configFlags'
  initPackageDBIfNeeded verbosity configFlags'' comp conf

  configure verbosity
            (configPackageDB' configFlags'') (globalRepos globalFlags')
            comp conf configFlags'' configExFlags' extraArgs

-- | Entry point for the 'cabal sandbox-build' command.
sandboxBuild :: Verbosity -> SandboxFlags -> BuildFlags -> [String] -> IO ()
sandboxBuild verbosity sandboxFlags buildFlags' extraArgs = do
  -- Check that the sandbox exists.
  _ <- getSandboxLocation verbosity sandboxFlags

  let setupScriptOptions = defaultSetupScriptOptions {
        useDistPref = fromFlagOrDefault
                      (useDistPref defaultSetupScriptOptions)
                      (buildDistPref buildFlags)
        }
      buildFlags = buildFlags' {
        buildVerbosity = toFlag verbosity
        }
  setupWrapper verbosity setupScriptOptions Nothing
    (buildCommand defaultProgramConfiguration) (const buildFlags) extraArgs

-- | Entry point for the 'cabal sandbox-install' command.
sandboxInstall :: Verbosity -> SandboxFlags -> ConfigFlags -> ConfigExFlags
                  -> InstallFlags -> HaddockFlags -> [String] -> GlobalFlags
                  -> IO ()
sandboxInstall verbosity _sandboxFlags _configFlags _configExFlags
  installFlags _haddockFlags _extraArgs _globalFlags
  | fromFlagOrDefault False (installOnly installFlags)
  -- TODO: It'd nice if this picked up the -w flag passed to sandbox-configure.
  -- Right now, running
  --
  -- $ cabal sandbox-init && cabal sandbox-configure -w /path/to/ghc
  --   && cabal sandbox-build && cabal sandbox-install
  --
  -- performs the compilation twice unless you also pass -w to sandbox-install.
  = setupWrapper verbosity defaultSetupScriptOptions Nothing
    installCommand (const mempty) []

sandboxInstall verbosity sandboxFlags configFlags configExFlags
  installFlags haddockFlags extraArgs globalFlags = do
  sandboxDir <- getSandboxLocation verbosity sandboxFlags
  pkgEnvDir  <- getCurrentDirectory
  pkgEnv     <- tryLoadPackageEnvironment verbosity sandboxDir pkgEnvDir
  targets    <- readUserTargets verbosity extraArgs

  let config        = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = defaultConfigExFlags         `mappend`
                       savedConfigureExFlags config `mappend` configExFlags
      installFlags'  = defaultInstallFlags          `mappend`
                       savedInstallFlags     config `mappend` installFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, conf) <- configCompilerAux' configFlags'

  -- If the user has set the -w option, we may need to create the package DB for
  -- this compiler.
  let configFlags''  = setPackageDB sandboxDir comp configFlags'
  initPackageDBIfNeeded verbosity configFlags'' comp conf

  install verbosity
          (configPackageDB' configFlags'') (globalRepos globalFlags')
          comp conf
          globalFlags' configFlags'' configExFlags' installFlags' haddockFlags
          targets

configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
  interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = fromFlagOrDefault True (configUserInstall cfg)

configCompilerAux' :: ConfigFlags
                      -> IO (Compiler, ProgramConfiguration)
configCompilerAux' configFlags =
  configCompilerAux configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
