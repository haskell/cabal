{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Distribution.Client.SetupHooks.HooksExe.Errors
  ( SetupHooksExeException (..)
  , BadHooksExecutableArgs (..)
  , setupHooksExeExceptionCode
  , setupHooksExeExceptionMessage
  ) where

-- Cabal
import Distribution.Simple.SetupHooks.Rule
  ( RuleId (..) )
import Distribution.Simple.Utils

-- base
import GHC.Exception
import GHC.Int
  ( Int64 )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

--------------------------------------------------------------------------------

data SetupHooksExeException
  = -- | Missing hook type.
    NoHookType
  | -- | Could not parse communication handle.
    NoHandle (Maybe String)
  | -- | Incorrect arguments passed to the hooks executable.
    BadHooksExeArgs
      String
      -- ^ hook name
      BadHooksExecutableArgs
  deriving (Show)

-- | An error describing an invalid argument passed to an external
-- hooks executable compiled from the @SetupHooks@ module of a package with
-- Hooks build-type.
data BadHooksExecutableArgs
  = -- | User queried the external hooks executable with an unknown hook type.
    UnknownHookType
      { knownHookTypes :: [String] }
  | -- | The hooks executable failed to decode the input passed to
    -- a particular hook.
    CouldNotDecodeInput
        ByteString
          -- ^ hook input that we failed to decode
        Int64
          -- ^ byte offset at which the decoding error took place
        String
          -- ^ info about the decoding error
  | -- | The rule does not have a dynamic dependency computation.
    NoDynDepsCmd RuleId
  deriving (Show)

setupHooksExeExceptionCode :: SetupHooksExeException -> Int
setupHooksExeExceptionCode = \case
  NoHookType -> 7982
  NoHandle {} -> 8811
  BadHooksExeArgs _ rea ->
    badHooksExeArgsCode rea

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
  CouldNotDecodeInput {} -> 9121
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
    "Failed to decode the input to the " ++ hookName ++ " hook.\n\
    \Decoding failed at position " ++ show offset ++ " with error: " ++ err ++ ".\n\
    \This could be due to a mismatch between the Cabal version of cabal-install and of the hooks executable."
  NoDynDepsCmd rId ->
    unlines $
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
