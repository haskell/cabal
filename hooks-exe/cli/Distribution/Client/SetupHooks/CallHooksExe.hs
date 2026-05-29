{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Distribution.Simple.Build
  ( builtinPreBuildHooks )
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
import Distribution.Verbosity
  ( Verbosity, VerbosityHandles, mkVerbosity )

-- hooks-cli
import Distribution.Client.SetupHooks.CallHooksExe.Errors
import Distribution.Simple.SetupHooks.HooksMain
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
  => Verbosity
  -> FilePath -- ^ path to hooks executable
  -> String   -- ^ name of the hook to run
  -> inputs   -- ^ argument to the hook
  -> IO outputs
callHooksExe verb hooksExe hookName input = do
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
      dieWithException verb $
        HookFailed hookName $
          HookException exitCode
    ExitSuccess -> do
      let mbOutput = Binary.decodeOrFail output
      case mbOutput of
        Left (_, offset, err) -> do
          dieWithException verb $
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
externalSetupHooks :: Verbosity -> FilePath -> SetupHooks
externalSetupHooks verb hooksExe =
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
    hook = callHooksExe verb hooksExe

-- | The ABI of an external hooks executable.
--
-- This information is used to handshake before further communication,
-- in order to avoid a cascade of errors with mismatched 'Binary' instances.
externalSetupHooksABI :: Verbosity -> FilePath -> IO HooksVersion
externalSetupHooksABI verb hooksExe =
  callHooksExe verb hooksExe "version" ()

-- | The 'SetupHooks' associated to a particular 'BuildType'.
--
-- **Warning:** for @build-type: Hooks@, this does not include the pre-build
-- hooks. Those can be retrieved with 'buildTypePreBuildHooks'.
buildTypeSetupHooks
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> BuildType
  -> SetupHooks
buildTypeSetupHooks verb mbWorkDir distPref = \case
  Hooks -> externalSetupHooks verb $ hooksProgFilePath mbWorkDir distPref
  Configure -> autoconfSetupHooks
  _ -> noSetupHooks
    -- Note: if any built-in functionality is implemented using SetupHooks,
    -- we would also need to include those.

-- | The pre-build hooks obtained by communication with an external hooks executable.
buildTypePreBuildHooks
  :: VerbosityHandles
  -> Maybe (SymbolicPath CWD (Dir Pkg))
  -> SymbolicPath Pkg (Dir Dist)
  -> BuildType
  -> ( PreBuildComponentInputs -> IO [MonitorFilePath] )
buildTypePreBuildHooks verbHandles mbWorkDir distPref bt pbci = do
  builtinMons <- builtinPreBuildHooks bt pbci
  externalMons <- case bt of
    Hooks ->
      runExternalPreBuildRules verbHandles
        (hooksProgFilePath mbWorkDir distPref)
        pbci
    _ -> return []
  return (builtinMons ++ externalMons)

-- | Run pre-build rules coming from an external hooks executable at the
-- given filepath.
--
-- Note that 'executeRulesUserOrSystem' handles recompilation checking, only
-- re-running rules that are stale.
runExternalPreBuildRules
  :: VerbosityHandles
  -> FilePath -- ^ path to external hooks executable
  -> PreBuildComponentInputs
  -> IO [MonitorFilePath]
runExternalPreBuildRules verbHandles hooksExe
  pbci@PreBuildComponentInputs
    { buildingWhat = what
    , localBuildInfo = lbi
    , targetInfo = tgt } = do
  let verbFlags = buildingWhatVerbosity what
      verbosity = mkVerbosity verbHandles verbFlags
      hook :: HookIO inputs outputs => String -> inputs -> IO outputs
      hook = callHooksExe verbosity hooksExe
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
    verbosity lbi tgt monitors rulesMap
  return monitors

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
