{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Client.SetupHooks where

import qualified Distribution.Compat.Binary as Binary

import Distribution.Client.Compat.Prelude
import Distribution.Client.Errors
import Distribution.Client.SetupHooks.Errors
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
import Distribution.Types.Component
import qualified Distribution.Types.LocalBuildConfig as LBC

import qualified Distribution.Verbosity as Verbosity

import Control.Concurrent
import Control.Exception
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Writer.CPS as MTL
import Data.ByteString.Lazy as LBS
  ( getContents
  , hGetContents
  , hPut
  , hPutStr
  , putStr
  )
import Data.Maybe
  ( fromJust
  )
import System.Environment (getArgs)
import System.IO (hClose, stderr)

import qualified System.Process as P

import qualified Data.Map as Map

-- | Create an executable which accepts the name of a hook as the argument,
-- then reads arguments to the hook over stdin and writes the results of the hook
-- to stdout.
hooksMain
  :: SetupHooks
  -> IO ()
-- See Note [Fine-grained hooks] in Distribution.Simple.SetupHooks
hooksMain setupHooks = do
  args <- getArgs
  case args of
    [] -> errorOut MissingHooksExeArg
    hookName : _hookArgs ->
      case lookup hookName allHookHandlers of
        Just handleAction -> handleAction setupHooks
        Nothing ->
          errorOut $
            BadHooksExeArgs hookName $
              UnknownHookType
                { knownHookTypes = map fst allHookHandlers
                }
  where
    errorOut err =
      dieWithException Verbosity.normal $
        SetupHooksExeException err

    allHookHandlers =
      [ (nm, action)
      | HookHandler
          { hookName = nm
          , hookHandler = action
          } <-
          hookHandlers
      ]

-- | Run a hook executable, passing inputs via stdin
-- and getting results from stdout.
runHookHandle
  :: forall inputs outputs
   . (Binary inputs, Binary outputs, Show inputs, Show outputs)
  => String
  -- ^ Hook name
  -> (inputs -> IO outputs)
  -- ^ Hook to run; inputs are passed via stdin
  -> IO ()
runHookHandle hookName hook = do
  inputsData <- LBS.getContents
  hPutStr stderr ("runHook " <> fromString hookName <> ": got stdin\n")
  let mb_inputs = Binary.decodeOrFail inputsData
  case mb_inputs of
    Left _ -> exitWith $ ExitFailure 99
    Right (_, _, inputs) -> do
      -- hPrint stderr inputs
      output <- hook inputs
      hPutStr stderr ("runHook " <> fromString hookName <> ": ran hook\n")
      LBS.putStr $ Binary.encode output

data HookHandler = HookHandler
  { hookName :: !String
  , hookHandler :: SetupHooks -> IO ()
  }

hookHandlers :: [HookHandler]
hookHandlers =
  [ let hookName = "preConfPackage"
        noHook (PreConfPackageInputs{localBuildConfig = lbc}) =
          return $
            PreConfPackageOutputs
              { buildOptions = LBC.withBuildOptions lbc
              , extraConfiguredProgs = Map.empty
              }
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide pre-configure hook.
          runHookHandle hookName $ fromMaybe noHook preConfPackageHook
  , let hookName = "postConfPackage"
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run the package-wide post-configure hook.
          for_ postConfPackageHook $ runHookHandle hookName
  , let hookName = "preConfComponent"
        noHook (PreConfComponentInputs{component = c}) =
          return $ PreConfComponentOutputs{componentDiff = emptyComponentDiff $ componentName c}
     in HookHandler hookName $ \(SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          -- Run a per-component pre-configure hook; the choice of component
          -- is determined by the input passed to the hook.
          runHookHandle hookName $ fromMaybe noHook preConfComponentHook
  , let hookName = "preBuildRules"
     in HookHandler hookName $ \(SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Return all pre-build rules.
          runHookHandle hookName $ \preBuildInputs ->
            case preBuildComponentRules of
              Nothing -> return (Map.empty, [])
              Just pbcRules ->
                computeRules Verbosity.normal preBuildInputs pbcRules
  , let hookName = "runPreBuildRuleDeps"
     in HookHandler hookName $ \_ ->
          -- Run the given pre-build rule dependency computation.
          runHookHandle hookName $ \(ruleId, ruleDeps) ->
            case runRuleDynDepsCmd ruleDeps of
              Nothing -> errorOut $ BadHooksExeArgs hookName $ NoDynDepsCmd ruleId
              Just getDeps -> getDeps
  , let hookName = "runPreBuildRule"
     in HookHandler hookName $ \_ ->
          -- Run the given pre-build rule.
          runHookHandle hookName $ \(_ruleId :: RuleId, rExecCmd) ->
            runRuleExecCmd rExecCmd
  , let hookName = "postBuildComponent"
     in HookHandler hookName $ \(SetupHooks{buildHooks = BuildHooks{..}}) ->
          -- Run the per-component post-build hook.
          for_ postBuildComponentHook $ runHookHandle hookName
  , let hookName = "installComponent"
     in HookHandler hookName $ \(SetupHooks{installHooks = InstallHooks{..}}) ->
          -- Run the per-component copy/install hook.
          for_ installComponentHook $ runHookHandle hookName
  ]
  where
    errorOut err =
      dieWithException Verbosity.normal $
        SetupHooksExeException err

-- SetupHooks TODO: this is for internal testing of the external hooks executable.
--
-- It's convenient to compute a value of type 'SetupHooks' as we can then
-- pass that back in to everything else. In practice we won't need to do this.
externalSetupHooks :: FilePath -> IO SetupHooks
externalSetupHooks hooksExe = do
  let
    callHooksExe :: (Binary inputs, Binary outputs) => String -> inputs -> IO outputs
    callHooksExe hookName inputs =
      P.withCreateProcess ((P.proc hooksExe [hookName]){P.std_in = P.CreatePipe, P.std_out = P.CreatePipe, P.std_err = P.Inherit}) $
        \mb_std_in mb_std_out _ ph -> do
          let std_in = fromJust mb_std_in
              std_out = fromJust mb_std_out

          -- fork off a thread to start consuming the output
          putStrLn $ "I am doing " ++ hookName
          output <- hGetContents std_out
          withForkWait (evaluate $ rnf output) $ \waitOut -> do
            -- now write any input
            ignoreSigPipe $ hPut std_in $ Binary.encode inputs
            -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
            ignoreSigPipe $ hClose std_in

            -- wait on the output
            waitOut
            hClose std_out

          ex <- P.waitForProcess ph
          putStrLn $ "I am done waiting on " ++ hookName
          case ex of
            ExitSuccess -> do
              putStrLn $ "Inner process succeeded for " ++ hookName
              return $ Binary.decode output
            ExitFailure{} -> error $ "Hooks executable failed to run " ++ hookName

    externalPreBuildRules :: PreBuildComponentInputs -> RulesM ()
    externalPreBuildRules pbci =
      -- Bypass the pre-build rules API, directly returning the pre-build
      -- rules obtained by querying the external hooks executable.
      --
      -- This is OK because we are not going to combine these pre-build rules
      -- with any other pre-build rules at this point; we have the entire
      -- collection of pre-build rules used by the package in hand now.
      RulesT $ do
        (rulesMap, monitors) <- MTL.liftIO $ callHooksExe "preBuildRules" pbci
        MTL.put rulesMap
        MTL.tell monitors

  return $
    SetupHooks
      { configureHooks =
          ConfigureHooks
            { preConfPackageHook = Just $ callHooksExe "preConfPackage"
            , postConfPackageHook = Just $ callHooksExe "postConfPackage"
            , preConfComponentHook = Just $ callHooksExe "preConfComponent"
            }
      , buildHooks =
          BuildHooks
            { preBuildComponentRules = Just $ Rules externalPreBuildRules
            , postBuildComponentHook = Just $ callHooksExe "postBuildComponent"
            }
      , installHooks =
          InstallHooks
            { installComponentHook = Just $ callHooksExe "installComponent"
            }
      }

withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid
