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

import Distribution.Client.Sandbox.PackageEnvironment (sandboxPackageDBPath)
import Distribution.Client.Sandbox.Types              (UseSandbox (..))
import Distribution.Client.Sandbox              (withSandboxBinDirOnSearchPath)

import Distribution.Simple.Compiler    (Compiler)
import Distribution.Simple.GHC         (ghcGlobalPackageDB)
import Distribution.Simple.Program     (ghcProgram, lookupProgram)
import Distribution.Simple.Program.Db  (ProgramDb)
import Distribution.Simple.Program.Run (getEffectiveEnvironment)
import Distribution.Simple.Utils       (debug, die, rawSystemExit,
                                        rawSystemExitWithEnv)

import Distribution.System    (Platform)
import Distribution.Verbosity (Verbosity)

import System.Exit (exitFailure)
import System.FilePath (searchPathSeparator)
import Control.Applicative ((<$>))
import Data.Traversable as T


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
            case useSandbox of
                NoSandbox ->
                    rawSystemExit verbosity exe args
                (UseSandbox sandboxDir) -> do
                    withSandboxBinDirOnSearchPath sandboxDir $ do
                        menv <- sandboxEnvironment
                                    verbosity sandboxDir comp platform programDb
                        case menv of
                            Just env ->
                                rawSystemExitWithEnv verbosity exe args env
                            Nothing  ->
                                rawSystemExit        verbosity exe args

        [] -> die $ "Please specify an executable to run"



-- | Return the package's sandbox environment.
--
-- The environment sets GHC_PACKAGE_PATH so that GHC will use the sandbox.
sandboxEnvironment :: Verbosity
                   -> FilePath
                   -> Compiler
                   -> Platform
                   -> ProgramDb
                   -> IO (Maybe [(String, String)])
sandboxEnvironment verbosity sandboxDir comp platform programDb = do
    mGlobalPackageDb <- T.sequence $ ghcGlobalPackageDB verbosity
                                  <$> lookupProgram ghcProgram programDb
    case mGlobalPackageDb of
        Nothing  -> do
            debug verbosity "exec only works with GHC"
            exitFailure
        Just gDb ->
            getEffectiveEnvironment $ overrides gDb
  where
    overrides gDb = [ ("GHC_PACKAGE_PATH", ghcPackagePath gDb) ]

    ghcPackagePath gDb =
        let s = sandboxPackageDBPath sandboxDir comp platform
            in Just $ prependToSearchPath gDb s

    prependToSearchPath path newValue =
        newValue ++ [searchPathSeparator] ++ path
