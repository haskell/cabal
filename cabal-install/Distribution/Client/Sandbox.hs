-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- UI for the sandboxing functionality.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox (
    dumpPackageEnvironment,

    sandboxAddSource,
    sandboxConfigure,
    sandboxBuild,
    sandboxInstall
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
  , loadOrCreatePackageEnvironment, tryLoadPackageEnvironment
  , commentPackageEnvironment
  , showPackageEnvironmentWithComments, readPackageEnvironmentFile
  , basePackageEnvironment, defaultPackageEnvironmentFileName )
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
import Distribution.Simple.Utils              ( die, notice
                                              , createDirectoryIfMissingVerbose )
import Distribution.ParseUtils                ( ParseResult(..) )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import qualified Distribution.Client.Index as Index
import qualified Distribution.Simple.Register as Register
import Control.Monad                          ( unless, when )
import Data.Monoid                            ( mappend, mempty )
import System.Directory                       ( canonicalizePath
                                              , doesDirectoryExist
                                              , doesFileExist )
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
  pkgEnvExists <- doesFileExist $
                  sandboxDir </> defaultPackageEnvironmentFileName
  unless (dirExists && pkgEnvExists) $
    die ("No sandbox exists at " ++ sandboxDir)
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir
  return sandboxDir

-- | Return the name of the package index file for this package environment.
getIndexFilePath :: PackageEnvironment -> IO FilePath
getIndexFilePath pkgEnv = do
  let paths = globalLocalRepos . savedGlobalFlags . pkgEnvSavedConfig $ pkgEnv
  case paths of
    []  -> die $ "Distribution.Client.Sandbox.getIndexFilePath: " ++
           "no local repos found"
    [p] -> return $ p </> Index.defaultIndexFileName
    _   -> die $ "Distribution.Client.Sandbox.getIndexFilePath: " ++
           "too many local repos found"

-- | Entry point for the 'cabal dump-pkgenv' command.
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> IO ()
dumpPackageEnvironment verbosity sandboxFlags = do
  pkgEnvDir     <- getSandboxLocation verbosity sandboxFlags
  pkgEnv        <- tryLoadPackageEnvironment verbosity pkgEnvDir
  commentPkgEnv <- commentPackageEnvironment pkgEnvDir
  putStrLn . showPackageEnvironmentWithComments commentPkgEnv $ pkgEnv

-- | Entry point for the 'cabal sandbox-configure' command.
sandboxConfigure :: Verbosity -> SandboxFlags -> ConfigFlags -> ConfigExFlags
                    -> [String] -> GlobalFlags -> IO ()
sandboxConfigure verbosity
  sandboxFlags configFlags configExFlags extraArgs globalFlags = do
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  createDirectoryIfMissingVerbose verbosity True sandboxDir'
  sandboxDir   <- canonicalizePath sandboxDir'
  (comp, conf) <- configCompilerSandbox sandboxDir
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir

  pkgEnv <- loadOrCreatePackageEnvironment verbosity sandboxDir configFlags comp

  let config         = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = savedConfigureExFlags config `mappend` configExFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
      [Just (SpecificPackageDB dbPath)]
                     = configPackageDBs configFlags'

  indexFile <- getIndexFilePath pkgEnv
  Index.createEmpty verbosity indexFile
  packageDBExists <- doesDirectoryExist dbPath
  unless packageDBExists $
    Register.initPackageDB verbosity comp conf dbPath
  when packageDBExists $
    notice verbosity $ "The package database already exists: " ++ dbPath
  configure verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
            comp conf configFlags' configExFlags' extraArgs
  where
    -- We need to know the compiler version so that the correct package DB is
    -- used. We try to read it from the package environment file, which might
    -- not exist.
    configCompilerSandbox :: FilePath -> IO (Compiler, ProgramConfiguration)
    configCompilerSandbox sandboxDir = do
      -- Build a ConfigFlags record...
      let basePkgEnv = basePackageEnvironment sandboxDir
      userConfig    <- loadConfig verbosity NoFlag NoFlag
      mPkgEnv       <- readPackageEnvironmentFile mempty
                       (sandboxDir </> defaultPackageEnvironmentFileName)
      let pkgEnv     = case mPkgEnv of
            Just (ParseOk _warns parseResult) -> parseResult
            _                                 -> mempty
      let basePkgEnvConfig = pkgEnvSavedConfig basePkgEnv
          pkgEnvConfig     = pkgEnvSavedConfig pkgEnv
          configFlags'     = savedConfigureFlags basePkgEnvConfig
                             `mappend` savedConfigureFlags userConfig
                             `mappend` savedConfigureFlags pkgEnvConfig
                             `mappend` configFlags
      -- ...and pass it to configCompilerAux.
      configCompilerAux configFlags'

-- | Entry point for the 'cabal sandbox-add-source' command.
sandboxAddSource :: Verbosity -> SandboxFlags -> [FilePath] -> IO ()
sandboxAddSource verbosity sandboxFlags buildTreeRefs = do
  sandboxDir <- getSandboxLocation verbosity sandboxFlags
  pkgEnv     <- tryLoadPackageEnvironment verbosity sandboxDir
  indexFile  <- getIndexFilePath pkgEnv
  Index.addBuildTreeRefs verbosity indexFile buildTreeRefs

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
  = setupWrapper verbosity defaultSetupScriptOptions Nothing
    installCommand (const mempty) []

sandboxInstall verbosity sandboxFlags configFlags configExFlags
  installFlags haddockFlags extraArgs globalFlags = do
  sandboxDir <- getSandboxLocation verbosity sandboxFlags

  pkgEnv <- tryLoadPackageEnvironment verbosity sandboxDir
  targets    <- readUserTargets verbosity extraArgs
  let config        = pkgEnvSavedConfig pkgEnv
      configFlags'   = savedConfigureFlags   config `mappend` configFlags
      configExFlags' = defaultConfigExFlags         `mappend`
                       savedConfigureExFlags config `mappend` configExFlags
      installFlags'  = defaultInstallFlags          `mappend`
                       savedInstallFlags     config `mappend` installFlags
      globalFlags'   = savedGlobalFlags      config `mappend` globalFlags
  (comp, conf) <- configCompilerAux' configFlags'
  install verbosity
          (configPackageDB' configFlags') (globalRepos globalFlags')
          comp conf
          globalFlags' configFlags' configExFlags' installFlags' haddockFlags
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
