{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Distribution.Client.SetupHooks.HooksExe.Errors
  ( SetupHooksExeException (..)
  , BadHooksExecutableArgs (..)
  , setupHooksExeExceptionCode
  , setupHooksExeExceptionMessage
  ) where

import Distribution.Simple.SetupHooks.Rule (RuleId (..))
import Distribution.Simple.Utils
import GHC.Exception

import Data.ByteString.Lazy (ByteString)

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
    CouldNotDecode
      { couldNotDecodeWhat :: String
        -- ^ A description of what it is that we failed to decode.
      , couldNotDecodeData :: ByteString
        -- ^ The actual data that we failed to decode.
      }
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
    \Expected three arguments: input and output communication handles, and hook type."
  NoHandle Nothing ->
    "Missing argument to Hooks executable.\n\
    \Expected three arguments: input and output communication handles, and hook type."
  NoHandle (Just h) ->
    "Invalid " ++ what ++ " passed to Hooks executable."
  BadHooksExeArgs hookName reason ->
    badHooksExeArgsMessage hookName reason

badHooksExeArgsCode :: BadHooksExecutableArgs -> Int
badHooksExeArgsCode = \case
  UnknownHookType{} -> 4229
  CouldNotDecode {} -> 9121
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
  CouldNotDecode { couldNotDecodeWhat = what } ->
    "Failed to decode " ++ what ++ " of " ++ hookName ++ " hook.\n\
    \This could be due to a mismatch between the Cabal version of cabal-install and of the hooks executable."
  NoDynDepsCmd rId ->
    unlines $
      [ "Unexpected rule " <> show rId <> " in" <> hookName
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
