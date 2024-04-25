{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Client.CmdLegacy (legacyCmd, legacyWrapperCmd, newCmd) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Sandbox
  ( findSavedDistPref
  , loadConfigOrSandboxConfig
  )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.SetupWrapper
  ( SetupRunnerArgs (NotInLibrary)
  , SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , setupWrapper
  )
import Distribution.Simple.Command
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Utils
  ( wrapText
  )
import Distribution.Verbosity
  ( normal
  )

import Control.Exception
  ( try
  )
import qualified Data.Text as T

-- Tweaked versions of code from Main.
regularCmd :: HasVerbosity flags => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> CommandSpec (globals -> IO action)
regularCmd ui action =
  CommandSpec ui ((flip commandAddAction) (\flags extra globals -> action flags extra globals)) NormalCommand

wrapperCmd
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Setup.CommonSetupFlags)
  -> CommandSpec (Client.GlobalFlags -> IO ())
wrapperCmd ui getCommonFlags =
  CommandSpec ui (\ui' -> wrapperAction ui' getCommonFlags) NormalCommand

wrapperAction
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Setup.CommonSetupFlags)
  -> Command (Client.GlobalFlags -> IO ())
wrapperAction command getCommonFlags =
  commandAddAction
    command
      { commandDefaultFlags = mempty
      }
    $ \flags extraArgs globalFlags -> do
      let common = getCommonFlags flags
          verbosity' = Setup.fromFlagOrDefault normal (Setup.setupVerbosity common)
          mbWorkDir = Setup.flagToMaybe $ Setup.setupWorkingDir common

      load <- try (loadConfigOrSandboxConfig verbosity' globalFlags)
      let config = either (\(SomeException _) -> mempty) id load
      distPref <- findSavedDistPref config (Setup.setupDistPref common)
      let setupScriptOptions =
            defaultSetupScriptOptions
              { useDistPref = distPref
              , useWorkingDir = mbWorkDir
              }

      let command' = command{commandName = T.unpack . T.replace "v1-" "" . T.pack . commandName $ command}

      setupWrapper
        verbosity'
        setupScriptOptions
        Nothing
        command'
        getCommonFlags
        (const flags)
        (const extraArgs)
        NotInLibrary

--

class HasVerbosity a where
  verbosity :: a -> Verbosity

instance HasVerbosity (Setup.Flag Verbosity) where
  verbosity = Setup.fromFlagOrDefault normal

instance HasVerbosity a => HasVerbosity (a, b) where
  verbosity (a, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c) where
  verbosity (a, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d) where
  verbosity (a, _, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d, e) where
  verbosity (a, _, _, _, _) = verbosity a

instance HasVerbosity a => HasVerbosity (a, b, c, d, e, f) where
  verbosity (a, _, _, _, _, _) = verbosity a

instance HasVerbosity Setup.BuildFlags where
  verbosity = verbosity . Setup.setupVerbosity . Setup.buildCommonFlags

instance HasVerbosity Setup.ConfigFlags where
  verbosity = verbosity . Setup.setupVerbosity . Setup.configCommonFlags

instance HasVerbosity Setup.ReplFlags where
  verbosity = verbosity . Setup.setupVerbosity . Setup.replCommonFlags

instance HasVerbosity Client.FreezeFlags where
  verbosity = verbosity . Client.freezeVerbosity

instance HasVerbosity Setup.HaddockFlags where
  verbosity = verbosity . Setup.setupVerbosity . Setup.haddockCommonFlags

instance HasVerbosity Client.UpdateFlags where
  verbosity = verbosity . Client.updateVerbosity

instance HasVerbosity Setup.CleanFlags where
  verbosity = verbosity . Setup.setupVerbosity . Setup.cleanCommonFlags

--

legacyNote :: String -> String
legacyNote cmd =
  wrapText $
    "The v1-"
      ++ cmd
      ++ " command is a part of the legacy v1 style of cabal usage.\n\n"
      ++ "It is a legacy feature and will be removed in a future release of cabal-install."
      ++ " Please file a bug if you cannot replicate a working v1- use case with the nix-style"
      ++ " commands.\n\n"
      ++ "For more information, see: https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html"

toLegacyCmd :: CommandSpec (globals -> IO action) -> [CommandSpec (globals -> IO action)]
toLegacyCmd mkSpec = [toLegacy mkSpec]
  where
    toLegacy (CommandSpec origUi@CommandUI{..} action type') = CommandSpec legUi action type'
      where
        legUi =
          origUi
            { commandName = "v1-" ++ commandName
            , commandNotes = Just $ \pname -> case commandNotes of
                Just notes -> notes pname ++ "\n" ++ legacyNote commandName
                Nothing -> legacyNote commandName
            }

legacyCmd :: HasVerbosity flags => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
legacyCmd ui action = toLegacyCmd (regularCmd ui action)

legacyWrapperCmd
  :: Monoid flags
  => CommandUI flags
  -> (flags -> Setup.CommonSetupFlags)
  -> [CommandSpec (Client.GlobalFlags -> IO ())]
legacyWrapperCmd ui commonFlags = toLegacyCmd (wrapperCmd ui commonFlags)

newCmd :: CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
newCmd origUi@CommandUI{..} action = [cmd defaultUi, cmd newUi, cmd origUi]
  where
    cmd ui = CommandSpec ui (flip commandAddAction action) NormalCommand

    newMsg = T.unpack . T.replace "v2-" "new-" . T.pack
    newUi =
      origUi
        { commandName = newMsg commandName
        , commandUsage = newMsg . commandUsage
        , commandDescription = (newMsg .) <$> commandDescription
        , commandNotes = (newMsg .) <$> commandNotes
        }

    defaultMsg = T.unpack . T.replace "v2-" "" . T.pack
    defaultUi =
      origUi
        { commandName = defaultMsg commandName
        , commandUsage = defaultMsg . commandUsage
        , commandDescription = (defaultMsg .) <$> commandDescription
        , commandNotes = (defaultMsg .) <$> commandNotes
        }
