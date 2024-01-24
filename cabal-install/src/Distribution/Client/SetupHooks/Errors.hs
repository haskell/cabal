{-# LANGUAGE LambdaCase #-}

module Distribution.Client.SetupHooks.Errors
  ( SetupHooksExeException (..)
  , BadHooksExecutableArgs (..)
  , setupHooksExeExceptionCode
  , setupHooksExeExceptionMessage
  ) where

import Distribution.Simple.SetupHooks.Rule (RuleId (..))
import GHC.StaticPtr (StaticKey)

data SetupHooksExeException
  = -- | No argument passed to the hooks executable.
    MissingHooksExeArg
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
  = -- | Wrong number of arguments passed to the external hooks executable
    WrongNumberOfArgs [String]
  | -- | User queried the external hooks executable with an unknown hook type.
    UnknownHookType
      {knownHookTypes :: [String]}
  | -- | The specified 'ActionId' passed to the external hooks executable
    -- does not have a corresponding 'Action'.
    NoSuchAction StaticKey
  | -- | The rule does not have a dynamic dependency computation.
    NoDynDepsCmd RuleId
  deriving (Show)

setupHooksExeExceptionCode :: SetupHooksExeException -> Int
setupHooksExeExceptionCode = \case
  MissingHooksExeArg -> 7982
  BadHooksExeArgs _ rea ->
    badHooksExeArgsCode rea

setupHooksExeExceptionMessage :: SetupHooksExeException -> String
setupHooksExeExceptionMessage = \case
  MissingHooksExeArg ->
    "Missing argument to Hooks executable.\n\
    \Expected hook type as an argument."
  BadHooksExeArgs hookName reason ->
    badHooksExeArgsMessage hookName reason

badHooksExeArgsCode :: BadHooksExecutableArgs -> Int
badHooksExeArgsCode = \case
  WrongNumberOfArgs{} -> 2970
  NoSuchAction{} -> 8373
  UnknownHookType{} -> 4229
  NoDynDepsCmd{} -> 3231

badHooksExeArgsMessage :: String -> BadHooksExecutableArgs -> String
badHooksExeArgsMessage hookName = \case
  WrongNumberOfArgs _ ->
    "Incorrect number of arguments in "
      ++ hookName
      ++ ".\n\
         \Expected a single component name as an argument."
  NoSuchAction ident ->
    hookName ++ ": no action with identifier " ++ show ident ++ "."
  UnknownHookType knownHookNames ->
    "Unknown hook type "
      ++ hookName
      ++ ".\n\
         \Known hook types are: "
      ++ show knownHookNames
      ++ "."
  NoDynDepsCmd rId ->
    unlines $
      [ "Unexpected rule " <> show rId <> " in" <> hookName
      , "The rule does not have an associated dynamic dependency computation."
      ]
