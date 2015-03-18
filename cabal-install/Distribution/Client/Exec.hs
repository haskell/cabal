{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'exec' command. Runs an arbitrary executable in an
-- environment suitable for making use of the sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Exec ( exec
                                ) where

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS

import Distribution.Client.Sandbox (getSandboxConfigFilePath)
import Distribution.Client.Sandbox.PackageEnvironment (sandboxPackageDBPath)
import Distribution.Client.Sandbox.Types              (UseSandbox (..))

import Distribution.Simple.Compiler    (Compiler, CompilerFlavor(..), compilerFlavor)
import Distribution.Simple.Program     (ghcProgram, ghcjsProgram, lookupProgram)
import Distribution.Simple.Program.Db  (ProgramDb, requireProgram, modifyProgramSearchPath)
import Distribution.Simple.Program.Find (ProgramSearchPathEntry(..))
import Distribution.Simple.Program.Run (programInvocation, runProgramInvocation)
import Distribution.Simple.Program.Types ( simpleProgram, ConfiguredProgram(..) )
import Distribution.Simple.Utils       (die)

import Distribution.System    (Platform)
import Distribution.Verbosity (Verbosity)

import System.FilePath (searchPathSeparator, (</>))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
#endif


-- | Execute the given command in the package's environment.
--
-- The given command is executed with GHC configured to use the correct
-- package database and with the sandbox bin directory added to the PATH.
exec :: Verbosity
     -> UseSandbox
     -> Compiler
     -> Platform
     -> ProgramDb
     -> [String]
     -> IO ()
exec verbosity useSandbox comp platform programDb extraArgs =
    case extraArgs of
        (exe:args) -> do
            program <- requireProgram' verbosity useSandbox programDb exe
            env <- ((++) (programOverrideEnv program)) <$> environmentOverrides
            let invocation = programInvocation
                                 program { programOverrideEnv = env }
                                 args
            runProgramInvocation verbosity invocation

        [] -> die "Please specify an executable to run"
  where
    environmentOverrides = 
        case useSandbox of
            NoSandbox -> return []
            (UseSandbox sandboxDir) ->
                sandboxEnvironment verbosity sandboxDir comp platform programDb


-- | Return the package's sandbox environment.
--
-- The environment sets GHC_PACKAGE_PATH so that GHC will use the sandbox.
sandboxEnvironment :: Verbosity
                   -> FilePath
                   -> Compiler
                   -> Platform
                   -> ProgramDb
                   -> IO [(String, Maybe String)]
sandboxEnvironment verbosity sandboxDir comp platform programDb =
    case compilerFlavor comp of
      GHC   -> env GHC.getGlobalPackageDB   ghcProgram   "GHC_PACKAGE_PATH"
      GHCJS -> env GHCJS.getGlobalPackageDB ghcjsProgram "GHCJS_PACKAGE_PATH"
      _     -> die "exec only works with GHC and GHCJS"
  where
    env getGlobalPackageDB hcProgram packagePathEnvVar = do
        let Just program = lookupProgram hcProgram programDb
        gDb <- getGlobalPackageDB verbosity program
        sandboxConfigFilePath <- getSandboxConfigFilePath mempty
        let compilerPackagePath = hcPackagePath gDb
        return [ (packagePathEnvVar, compilerPackagePath)
               , ("CABAL_SANDBOX_PACKAGE_PATH", compilerPackagePath)
               , ("CABAL_SANDBOX_CONFIG", Just sandboxConfigFilePath)
               ]

    hcPackagePath gDb =
        let s = sandboxPackageDBPath sandboxDir comp platform
            in Just $ prependToSearchPath gDb s

    prependToSearchPath path newValue =
        newValue ++ [searchPathSeparator] ++ path


-- | Check that a program is configured and available to be run. If
-- a sandbox is available check in the sandbox's directory.
requireProgram' :: Verbosity
                -> UseSandbox
                -> ProgramDb
                -> String
                -> IO ConfiguredProgram
requireProgram' verbosity useSandbox programDb exe = do
    (program, _) <- requireProgram
                        verbosity
                        (simpleProgram exe)
                        updateSearchPath
    return program
  where
    updateSearchPath =
        flip modifyProgramSearchPath programDb $ \searchPath ->
            case useSandbox of
                NoSandbox -> searchPath
                UseSandbox sandboxDir ->
                    ProgramSearchPathDir (sandboxDir </> "bin") : searchPath
