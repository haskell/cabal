{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.SetupHooks.HooksExe
  ( hooksMain ) where

-- base
import System.Environment
  ( getArgs )
import System.IO
  ( Handle, hClose, hFlush )

-- bytestring
import Data.ByteString.Lazy as LBS
  ( hGetContents
  , hPutStr
  , null
  )

-- containers
import qualified Data.Map as Map

-- process
import System.Process.CommunicationHandle
  ( openCommunicationHandleRead
  , openCommunicationHandleWrite
  )

-- Cabal
import Distribution.Compat.Prelude
import qualified Distribution.Compat.Binary as Binary
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
  ( dieWithException )
import Distribution.Types.Component
  ( componentName )
import qualified Distribution.Types.LocalBuildConfig as LBC
import qualified Distribution.Verbosity as Verbosity

-- hooks-exe
import Distribution.Client.SetupHooks.HooksExe.Errors
  ( SetupHooksExeException(..)
  , BadHooksExecutableArgs(..)
  )
import Distribution.Client.SetupHooks.Version
  ( hooksVersion )

--------------------------------------------------------------------------------

-- | Create a hooks executable given 'SetupHooks':
--
--  - the first two argument are references to input & output communication
--    handles,
--  - the second argument is the hook type.
--
-- The hook reads binary data passed to it over the input handle, decodes it,
-- runs the hook, and encodes its result to binary, writing the result to the
-- output handle.
hooksMain :: SetupHooks -> IO ()
hooksMain setupHooks = do
  args <- getArgs
  case args of
    -- First two arguments are references to read/write handles the hooks executable should use.
    inputFdRef : outputFdRef : hooksExeArgs -> do
      hReadMb  <- traverse openCommunicationHandleRead  $ readMaybe inputFdRef
      hWriteMb <- traverse openCommunicationHandleWrite $ readMaybe outputFdRef
      case hReadMb of
        Nothing ->
          dieWithException Verbosity.normal $
            NoHandle (Just $ "hook input communication handle '" ++ inputFdRef ++ "'")
        Just hRead ->
          case hWriteMb of
            Nothing ->
              dieWithException Verbosity.normal $
                NoHandle (Just $ "hook output communication handle '" ++ outputFdRef ++ "'")
            Just hWrite ->
              -- Third argument is the hook to run.
              case hooksExeArgs of
                hookName : _ ->
                  case lookup hookName allHookHandlers of
                    Just handleAction ->
                      handleAction (hRead, hWrite) setupHooks
                    Nothing ->
                      dieWithException Verbosity.normal $
                        BadHooksExeArgs hookName $
                          UnknownHookType
                            { knownHookTypes = map fst allHookHandlers
                            }
                _ -> dieWithException Verbosity.normal NoHookType
    _ -> dieWithException Verbosity.normal $
           NoHandle Nothing
  where
    allHookHandlers =
      [ (nm, action)
      | HookHandler
          { hookName = nm
          , hookHandler = action
          } <-
          hookHandlers
      ]

-- | Implementation of a particular hook in a separate hooks executable,
-- which communicates through the given 'Handle's.
runHookHandle
  :: forall inputs outputs
   . (Binary inputs, Binary outputs)
  => (Handle, Handle)
  -- ^ Input/output communication handles
  -> String
  -- ^ Hook name
  -> (inputs -> IO outputs)
  -- ^ Hook to run
  --
  -- Inputs are passed via the input handle, and outputs are written to the
  -- output handle.
  -> IO ()
runHookHandle (hRead, hWrite) hookName hook = do
  inputsData <- LBS.hGetContents hRead
  let mb_inputs = Binary.decodeOrFail inputsData
  case mb_inputs of
    Left (_, offset, err) ->
      dieWithException Verbosity.normal $
        BadHooksExeArgs hookName $
          CouldNotDecodeInput inputsData offset err
    Right (_, _, inputs) -> do
      output <- hook inputs
      let outputData = Binary.encode output
      unless (LBS.null outputData) $
        LBS.hPutStr hWrite outputData
      hFlush hWrite
      hClose hWrite

data HookHandler = HookHandler
  { hookName :: !String
  , hookHandler :: (Handle, Handle) -> SetupHooks -> IO ()
  }

hookHandlers :: [HookHandler]
hookHandlers =
  [ let hookName = "version"
    in HookHandler hookName $ \h _ ->
          -- Print the API version and ABI hash for the hooks executable.
          runHookHandle h hookName $ \ () ->
            return $ hooksVersion
  , let hookName = "preConfPackage"
        noHook (PreConfPackageInputs{localBuildConfig = lbc}) =
          return $
            PreConfPackageOutputs
              { buildOptions = LBC.withBuildOptions lbc
              , extraConfiguredProgs = Map.empty
              }
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide pre-configure hook.
          runHookHandle h hookName $ fromMaybe noHook preConfPackageHook
  , let hookName = "postConfPackage"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide post-configure hook.
          runHookHandle h hookName $ fromMaybe noHook postConfPackageHook
  , let hookName = "preConfComponent"
        noHook (PreConfComponentInputs{component = c}) =
          return $ PreConfComponentOutputs{componentDiff = emptyComponentDiff $ componentName c}
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run a per-component pre-configure hook; the choice of component
          -- is determined by the input passed to the hook.
          runHookHandle h hookName $ fromMaybe noHook preConfComponentHook
  , let hookName = "preBuildRules"
     in HookHandler hookName $ \h (SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Return all pre-build rules.
          runHookHandle h hookName $ \preBuildInputs ->
            case preBuildComponentRules of
              Nothing -> return (Map.empty, [])
              Just pbcRules ->
                computeRules Verbosity.normal preBuildInputs pbcRules
  , let hookName = "runPreBuildRuleDeps"
     in HookHandler hookName $ \h _ ->
          -- Run the given pre-build rule dependency computation.
          runHookHandle h hookName $ \(ruleId, ruleDeps) ->
            case runRuleDynDepsCmd ruleDeps of
              Nothing -> dieWithException Verbosity.normal $ BadHooksExeArgs hookName $ NoDynDepsCmd ruleId
              Just getDeps -> getDeps
  , let hookName = "runPreBuildRule"
     in HookHandler hookName $ \h _ ->
          -- Run the given pre-build rule.
          runHookHandle h hookName $ \(_ruleId :: RuleId, rExecCmd) ->
            runRuleExecCmd rExecCmd
  , let hookName = "postBuildComponent"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Run the per-component post-build hook.
          runHookHandle h hookName $ fromMaybe noHook postBuildComponentHook
  , let hookName = "installComponent"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{installHooks = InstallHooks{..}}) ->
          -- Run the per-component copy/install hook.
          runHookHandle h hookName $ fromMaybe noHook installComponentHook
  ]
