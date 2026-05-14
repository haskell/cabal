{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of hooks executables for @build-type: Hooks@ packages.
--
-- A hooks executable is a small program compiled from the @SetupHooks.hs@
-- module of a package with @build-type: Hooks@. Its @main@ function is:
--
-- > import Distribution.Simple.SetupHooks.HooksMain (hooksMain)
-- > import SetupHooks (setupHooks)
-- > main = hooksMain setupHooks
--
-- @cabal-install@ communicates with the external hooks executable to implement
-- the hooks in a package with @build-type: Hooks@.
module Distribution.Simple.SetupHooks.HooksMain
  ( -- * Main entry point for hooks executables
    hooksMain

    -- * Hooks version handshake
  , HooksVersion (..)
  , hooksVersion
  , CabalABI (..)
  , HooksABI (..)
  ) where

-- base
import Control.Monad
  ( (>=>)
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import GHC.Exception
import System.Environment
  ( getArgs
  )
import System.IO
  ( Handle
  , hClose
  , hFlush
  )

-- bytestring
import Data.ByteString.Lazy as LBS
  ( ByteString
  , hGetContents
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

-- transformers
import Control.Monad.Trans.Except
  ( ExceptT
  , runExceptT
  , throwE
  )

-- Cabal-syntax
import qualified Distribution.Compat.Binary as Binary
  ( decodeOrFail
  , encode
  )
import Distribution.Types.Version
  ( Version
  )
import Distribution.Utils.Structured
  ( MD5
  , structureHash
  )

-- Cabal
import Distribution.Compat.Prelude
import Distribution.Simple.SetupHooks.Internal
import Distribution.Simple.SetupHooks.Rule
import Distribution.Simple.Utils
  ( VerboseException (..)
  , cabalVersion
  , dieWithException
  , exceptionWithMetadata
  , withOutputMarker
  )
import Distribution.Types.Component
  ( componentName
  )
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo
  )
import Distribution.Verbosity
  ( Verbosity
  , defaultVerbosityHandles
  , mkVerbosity
  )
import qualified Distribution.Verbosity as Verbosity
  ( normal
  )

--------------------------------------------------------------------------------
-- Hooks version

-- | The version of the Hooks API in use.
--
-- Used for handshake before beginning inter-process communication.
data HooksVersion = HooksVersion
  { hooksAPIVersion :: !Version
  , cabalABIHash :: !MD5
  , hooksABIHash :: !MD5
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | The version of the Hooks API built into this version of the Cabal library.
--
-- Used for handshake before beginning inter-process communication.
hooksVersion :: HooksVersion
hooksVersion =
  HooksVersion
    { hooksAPIVersion = cabalVersion
    , cabalABIHash = structureHash $ Proxy @CabalABI
    , hooksABIHash = structureHash $ Proxy @HooksABI
    }

-- | Tracks the parts of the Cabal API relevant to its binary interface.
data CabalABI = CabalABI
  { cabalLocalBuildInfo :: LocalBuildInfo
  }
  deriving stock (Generic)

deriving anyclass instance Structured CabalABI

-- | Tracks the parts of the Hooks API relevant to its binary interface.
data HooksABI = HooksABI
  { confHooks
      :: ( (PreConfPackageInputs, PreConfPackageOutputs)
         , PostConfPackageInputs
         , (PreConfComponentInputs, PreConfComponentOutputs)
         )
  , buildHooks
      :: ( PreBuildComponentInputs
         , (RuleId, Rule, RuleBinary)
         , PostBuildComponentInputs
         )
  , installHooks :: InstallComponentInputs
  }
  deriving stock (Generic)

deriving anyclass instance Structured HooksABI

--------------------------------------------------------------------------------
-- Error types (internal)

data SetupHooksExeException
  = -- | Missing hook type argument.
    NoHookType
  | -- | Could not parse a communication handle argument.
    NoHandle (Maybe String)
  | -- | Incorrect arguments passed to the hooks executable.
    BadHooksExeArgs
      String
      -- ^ hook name
      BadHooksExecutableArgs
  deriving (Show)

-- | An error describing an invalid argument passed to a hooks executable.
data BadHooksExecutableArgs
  = -- | Unknown hook type was requested.
    UnknownHookType
      {knownHookTypes :: [String]}
  | -- | Failed to decode the binary input to a hook.
    CouldNotDecodeInput
      ByteString
      -- ^ hook input that failed to decode
      Int64
      -- ^ byte offset at which decoding failed
      String
      -- ^ decoding error message
  | -- | The rule does not have a dynamic dependency computation.
    NoDynDepsCmd RuleId
  deriving (Show)

setupHooksExeExceptionCode :: SetupHooksExeException -> Int
setupHooksExeExceptionCode = \case
  NoHookType -> 7982
  NoHandle{} -> 8811
  BadHooksExeArgs _ rea -> badHooksExeArgsCode rea

setupHooksExeExceptionMessage :: SetupHooksExeException -> String
setupHooksExeExceptionMessage = \case
  NoHookType ->
    "Missing argument to Hooks executable.\n\
    \Expected two arguments: communication handle and hook type."
  NoHandle Nothing ->
    "Missing argument to Hooks executable.\n\
    \Expected two arguments: communication handle and hook type."
  NoHandle (Just h) ->
    "Invalid handle reference passed to Hooks executable: '" ++ h ++ "'."
  BadHooksExeArgs hookName reason ->
    badHooksExeArgsMessage hookName reason

badHooksExeArgsCode :: BadHooksExecutableArgs -> Int
badHooksExeArgsCode = \case
  UnknownHookType{} -> 4229
  CouldNotDecodeInput{} -> 9121
  NoDynDepsCmd{} -> 3231

badHooksExeArgsMessage :: String -> BadHooksExecutableArgs -> String
badHooksExeArgsMessage hookName = \case
  UnknownHookType knownHookNames ->
    "Unknown hook type "
      ++ hookName
      ++ ".\n\
         \Known hook types are: "
      ++ show knownHookNames
      ++ "."
  CouldNotDecodeInput _bytes offset err ->
    "Failed to decode the input to the "
      ++ hookName
      ++ " hook.\n\
         \Decoding failed at position "
      ++ show offset
      ++ " with error: "
      ++ err
      ++ ".\n\
         \This could be due to a mismatch between the Cabal version of cabal-install\
         \ and of the hooks executable."
  NoDynDepsCmd rId ->
    unlines
      [ "Unexpected rule " <> show rId <> " in the " <> hookName <> " hook."
      , "The rule does not have an associated dynamic dependency computation."
      ]

instance Exception (VerboseException SetupHooksExeException) where
  displayException :: VerboseException SetupHooksExeException -> String
  displayException (VerboseException stack timestamp verb err) =
    withOutputMarker
      verb
      ( concat
          [ "Error: [Cabal-"
          , show (setupHooksExeExceptionCode err)
          , "]\n"
          ]
      )
      ++ exceptionWithMetadata stack timestamp verb (setupHooksExeExceptionMessage err)

-- | The verbosity used inside the hooks executable.
--
-- The hooks executable is always invoked as a separate process, so stdout
-- and stderr are available for verbosity output and can be redirected via
-- the @System.Process@ API.
hooksExeVerbosity :: Verbosity
hooksExeVerbosity = mkVerbosity defaultVerbosityHandles Verbosity.normal

--------------------------------------------------------------------------------
-- Main entry point

-- | Create a hooks executable @main@ given the package's 'SetupHooks'.
--
-- The executable expects three command-line arguments:
--
--  1. A reference to an input communication handle (to read hook inputs from).
--  2. A reference to an output communication handle (to write hook outputs to).
--  3. The hook type to run.
--
-- The hook reads binary-encoded data from the input handle, runs the
-- requested hook, and writes the binary-encoded result to the output handle.
hooksMain :: SetupHooks -> IO ()
hooksMain setupHooks = runHooksM $ do
  ((hRead, hWrite), hookName) <- getHooksMainArgs
  case lookup hookName allHookHandlers of
    Just handleAction ->
      handleAction (hRead, hWrite) setupHooks
    Nothing ->
      throwE $
        BadHooksExeArgs hookName $
          UnknownHookType
            { knownHookTypes = map fst allHookHandlers
            }
  where
    allHookHandlers = [(hookName h, hookHandler h) | h <- hookHandlers]

    -- Get the communication handles and the name of the hook to run
    getHooksMainArgs :: HooksM ((Handle, Handle), String)
    getHooksMainArgs =
      liftIO getArgs >>= \case
        inputFdRef : outputFdRef : hookNm : _ ->
          case (readMaybe inputFdRef, readMaybe outputFdRef) of
            (Just readNm, Just writeNm) -> do
              hRead <- liftIO $ openCommunicationHandleRead readNm
              hWrite <- liftIO $ openCommunicationHandleWrite writeNm
              return ((hRead, hWrite), hookNm)
            (Nothing, _) ->
              throwE $ NoHandle (Just $ "hook input communication handle '" ++ inputFdRef ++ "'")
            (_, Nothing) ->
              throwE $ NoHandle (Just $ "hook output communication handle '" ++ outputFdRef ++ "'")
        _ -> throwE $ NoHandle Nothing

type HooksM = ExceptT SetupHooksExeException IO

runHooksM :: HooksM a -> IO a
runHooksM = runExceptT >=> either (dieWithException hooksExeVerbosity) pure

-- | Run a hook by reading its input from a handle, invoking it, and writing
-- its output to another handle.
runHookHandle
  :: forall inputs outputs
   . (Binary inputs, Binary outputs)
  => (Handle, Handle)
  -- ^ Input and output communication handles
  -> String
  -- ^ Hook name (used in error messages)
  -> (inputs -> HooksM outputs)
  -- ^ The hook to run
  -> HooksM ()
runHookHandle (hRead, hWrite) hookName hook = do
  inputsData <- liftIO $ LBS.hGetContents hRead
  let mb_inputs = Binary.decodeOrFail inputsData
  case mb_inputs of
    Left (_, offset, err) ->
      throwE $
        BadHooksExeArgs hookName $
          CouldNotDecodeInput inputsData offset err
    Right (_, _, inputs) ->
      hook inputs >>= \output -> liftIO $ do
        let outputData = Binary.encode output
        unless (LBS.null outputData) $
          LBS.hPutStr hWrite outputData
        hFlush hWrite
        hClose hWrite

data HookHandler = HookHandler
  { hookName :: !String
  , hookHandler :: (Handle, Handle) -> SetupHooks -> HooksM ()
  }

hookHandlers :: [HookHandler]
hookHandlers =
  [ let hookName = "version"
     in HookHandler hookName $ \h _ ->
          runHookHandle h hookName $ \() ->
            return hooksVersion
  , let hookName = "preConfPackage"
        noHook (PreConfPackageInputs{localBuildConfig = lbc}) =
          return $
            PreConfPackageOutputs
              { buildOptions = LBC.withBuildOptions lbc
              , extraConfiguredProgs = Map.empty
              }
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          runHookHandle h hookName $ maybe noHook (liftIO .) preConfPackageHook
  , let hookName = "postConfPackage"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          runHookHandle h hookName $ maybe noHook (liftIO .) postConfPackageHook
  , let hookName = "preConfComponent"
        noHook (PreConfComponentInputs{component = c}) =
          return $ PreConfComponentOutputs{componentDiff = emptyComponentDiff $ componentName c}
     in HookHandler hookName $ \h (SetupHooks{configureHooks = ConfigureHooks{..}}) ->
          runHookHandle h hookName $ maybe noHook (liftIO .) preConfComponentHook
  , let hookName = "preBuildRules"
     in HookHandler hookName $ \h (SetupHooks{buildHooks = BuildHooks{..}}) ->
          runHookHandle h hookName $ \preBuildInputs ->
            case preBuildComponentRules of
              Nothing -> return (Map.empty, [])
              Just pbcRules ->
                liftIO $
                  computeRules hooksExeVerbosity preBuildInputs pbcRules
  , let hookName = "runPreBuildRuleDeps"
     in HookHandler hookName $ \h _ ->
          runHookHandle h hookName $ \(ruleId, ruleDeps) ->
            case runRuleDynDepsCmd ruleDeps of
              Nothing ->
                throwE $
                  BadHooksExeArgs hookName $
                    NoDynDepsCmd ruleId
              Just getDeps -> liftIO getDeps
  , let hookName = "runPreBuildRule"
     in HookHandler hookName $ \h _ ->
          runHookHandle h hookName $ \(_ruleId :: RuleId, rExecCmd) ->
            liftIO $ runRuleExecCmd rExecCmd
  , let hookName = "postBuildComponent"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{buildHooks = BuildHooks{..}}) ->
          runHookHandle h hookName $ maybe noHook (liftIO .) postBuildComponentHook
  , let hookName = "installComponent"
        noHook _ = return ()
     in HookHandler hookName $ \h (SetupHooks{installHooks = InstallHooks{..}}) ->
          runHookHandle h hookName $ maybe noHook (liftIO .) installComponentHook
  ]
