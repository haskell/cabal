{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use curry" -}

module Distribution.Client.SetupHooks.CallHooksExe
  ( callHooksExe
  , externalSetupHooks
  , externalSetupHooksABI
  , buildTypeSetupHooks
  , buildTypePreBuildHooks
  , runExternalPreBuildRules
  , hooksProgFilePath
  ) where

-- base
import GHC.Stack

-- bytestring
import Data.ByteString.Lazy as LBS
  ( hGetContents
  , hPut
  , null
  )

-- process
import qualified System.Process as P
import System.Process.CommunicationHandle
  ( readCreateProcessWithExitCodeCommunicationHandle )

-- filepath
import System.FilePath
  ( (</>), (<.>) )

-- Cabal
import Distribution.Compat.Prelude
import qualified Distribution.Compat.Binary as Binary
import Distribution.Simple
  ( autoconfSetupHooks )
import Distribution.Simple.BuildPaths
  ( exeExtension )
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
  ( dieWithException )
import Distribution.System
  ( buildPlatform )
import Distribution.Types.BuildType
  ( BuildType(..) )
import Distribution.Utils.Path
  ( CWD
  , Dist
  , Pkg
  , SymbolicPath
  , FileOrDir(..)
  , interpretSymbolicPath
  )
import qualified Distribution.Verbosity as Verbosity

-- hooks-cli
import Distribution.Client.SetupHooks.CallHooksExe.Errors
import Distribution.Client.SetupHooks.Version
  ( HooksVersion )

--------------------------------------------------------------------------------

type HookIO inputs outputs =
  ( HasCallStack
  , Typeable inputs, Typeable outputs
  , Binary inputs, Binary outputs
  )

-- | Call an external hooks executable in order to execute a Cabal Setup hook.
callHooksExe
  :: forall inputs outputs
  .  HookIO inputs outputs
  => FilePath -- ^ path to hooks executable
  -> String   -- ^ name of the hook to run
  -> inputs   -- ^ argument to the hook
  -> IO outputs
callHooksExe hooksExe hookName input = do
  (ex, output) <-
    -- The arguments to the external hooks executable are:
    --
    --  1. Input handle, from which the hooks executable receives its input.
    --  2. Output handle, to which the hooks executable writes its output.
    --  3. The hook type to run.
    --
    -- The hooks executable will read input from the input handle, decode it,
    -- run the necessary hook, producing a result which it encodes and writes
    -- to the output handle.
    readCreateProcessWithExitCodeCommunicationHandle
      ( \(theyRead, theyWrite) -> P.proc hooksExe [show theyRead, show theyWrite, hookName] )
      ( \ hWeRead -> hGetContents hWeRead )
      ( \ hWeWrite -> do
        let i = Binary.encode input
        unless (LBS.null i) $
          hPut hWeWrite i
      )
  case ex of
    ExitFailure exitCode ->
      dieWithException Verbosity.normal $
        HookFailed hookName $
          HookException exitCode
    ExitSuccess -> do
      let mbOutput = Binary.decodeOrFail output
      case mbOutput of
        Left (_, offset, err) -> do
          dieWithException Verbosity.normal $
            HookFailed hookName $
              CouldNotDecodeOutput output offset err
        Right (_, _, res) -> return res

-- | Construct a 'SetupHooks' that runs the hooks of the external hooks executable
-- at the given path through the CLI.
--
-- This should only be used at the final step of compiling a package, when we
-- have all the hooks in hand. The SetupHooks that are returned by this function
-- cannot be combined with any other SetupHooks; they must directly be used to
-- build the package.
externalSetupHooks :: FilePath -> SetupHooks
externalSetupHooks hooksExe =
  SetupHooks
    { configureHooks =
        ConfigureHooks
          { preConfPackageHook = Just $ hook "preConfPackage"
          , postConfPackageHook = Just $ hook "postConfPackage"
          , preConfComponentHook = Just $ hook "preConfComponent"
          }
    , buildHooks =
        BuildHooks
          { -- NB: external pre-build rules are special, due to the StaticPtr machinery.
            -- To invoke them, we must separately call 'runExternalPreBuildRules'.
            preBuildComponentRules = Nothing
          , postBuildComponentHook = Just $ hook "postBuildComponent"
          }
    , installHooks =
        InstallHooks
          { installComponentHook = Just $ hook "installComponent"
          }
    }
  where
    hook :: HookIO inputs outputs => String -> inputs -> IO outputs
    hook = callHooksExe hooksExe

-- | The ABI of an external hooks executable.
--
-- This information is used to handshake before further communication,
-- in order to avoid a cascade of errors with mismatched 'Binary' instances.
externalSetupHooksABI :: FilePath -> IO HooksVersion
externalSetupHooksABI hooksExe =
  callHooksExe hooksExe "version" ()

-- | The 'SetupHooks' associated to a particular 'BuildType'.
--
-- **Warning:** for @build-type: Hooks@, this does not include the pre-build
-- hooks. Those can be retrieved with 'buildTypePreBuildHooks'.
buildTypeSetupHooks
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> BuildType
  -> SetupHooks
buildTypeSetupHooks mbWorkDir distPref = \case
  Hooks -> externalSetupHooks $ hooksProgFilePath mbWorkDir distPref
  Configure -> autoconfSetupHooks
  _ -> noSetupHooks
    -- SetupHooks TODO: if any built-in functionality is implemented using SetupHooks,
    -- we would also need to include those.

-- | The pre-build hooks obtained by communication with an external hooks executable.
buildTypePreBuildHooks
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> BuildType
  -> ( PreBuildComponentInputs -> IO [MonitorFilePath] )
buildTypePreBuildHooks mbWorkDir distPref = \ case
  Hooks -> runExternalPreBuildRules $ hooksProgFilePath mbWorkDir distPref
  _ -> \ _pbci -> return []
    -- SetupHooks TODO: if any built-in functionality is implemented using pre-build hooks,
    -- we would also need to include those (for example, pre-processors such as hsc2hs).

-- | Run all pre-build rules coming from an external hooks executable at the
-- given filepath.
--
-- TODO: in the future, we will want to keep track of the dependency graph ourselves,
-- and when re-building, only re-build what we need (instead of re-running all rules).
runExternalPreBuildRules :: FilePath -> PreBuildComponentInputs -> IO [MonitorFilePath]
runExternalPreBuildRules hooksExe
  pbci@PreBuildComponentInputs
    { buildingWhat = what
    , localBuildInfo = lbi
    , targetInfo = tgt } = do
  let verbosity = buildingWhatVerbosity what
  -- Here we make sure to use 'RuleBinary' (@'Scope' == 'System'@)
  -- to avoid looking up static pointer keys from the hooks executable
  -- from the outside (e.g. from within cabal-install).
  (rulesMap :: Map RuleId RuleBinary, monitors) <- hook "preBuildRules" pbci
  executeRulesUserOrSystem
    SSystem
    ( \ rId cmd -> case cmd of
      StaticRuleCommand {} -> return Nothing
      DynamicRuleCommands {} -> hook "runPreBuildRuleDeps" (rId, cmd)
    )
    ( \ rId cmd -> hook "runPreBuildRule" (rId, cmd) )
    verbosity lbi tgt rulesMap
  return monitors
  where
    hook :: HookIO inputs outputs => String -> inputs -> IO outputs
    hook = callHooksExe hooksExe

-- | The path to the external hooks executable.
hooksProgFilePath
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> FilePath
hooksProgFilePath mbWorkDir distPref =
  interpretSymbolicPath mbWorkDir distPref
    </> "setup"
    </> "hooks"
    <.> exeExtension buildPlatform
