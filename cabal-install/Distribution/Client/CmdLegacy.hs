{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.Client.CmdLegacy ( legacyCmd, legacyWrapperCmd, newCmd ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.Sandbox
    ( loadConfigOrSandboxConfig, findSavedDistPref )
import qualified Distribution.Client.Setup as Client
import Distribution.Client.SetupWrapper
    ( SetupScriptOptions(..), setupWrapper, defaultSetupScriptOptions )
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Command
import Distribution.Simple.Utils
    ( warn, wrapText )
import Distribution.Verbosity 
    ( Verbosity, normal )

import Control.Exception
    ( SomeException(..), try )
import qualified Data.Text as T

-- Tweaked versions of code from Main.
regularCmd :: (HasVerbosity flags) => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> Bool -> CommandSpec (globals -> IO action)
regularCmd ui action shouldWarn =
        CommandSpec ui ((flip commandAddAction) (\flags extra globals -> showWarning flags >> action flags extra globals)) NormalCommand
    where
        showWarning flags = if shouldWarn 
            then warn (verbosity flags) (deprecationNote (commandName ui) ++ "\n")
            else return ()

wrapperCmd :: Monoid flags => CommandUI flags -> (flags -> Setup.Flag Verbosity) -> (flags -> Setup.Flag String) -> Bool -> CommandSpec (Client.GlobalFlags -> IO ())
wrapperCmd ui verbosity' distPref shouldWarn =
  CommandSpec ui (\ui' -> wrapperAction ui' verbosity' distPref shouldWarn) NormalCommand

wrapperAction :: Monoid flags => CommandUI flags  -> (flags -> Setup.Flag Verbosity) -> (flags -> Setup.Flag String) -> Bool -> Command (Client.GlobalFlags -> IO ())
wrapperAction command verbosityFlag distPrefFlag shouldWarn =
  commandAddAction command
    { commandDefaultFlags = mempty } $ \flags extraArgs globalFlags -> do
    let verbosity' = Setup.fromFlagOrDefault normal (verbosityFlag flags)

    if shouldWarn
        then warn verbosity' (deprecationNote (commandName command) ++ "\n")
        else return ()

    load <- try (loadConfigOrSandboxConfig verbosity' globalFlags)
    let config = either (\(SomeException _) -> mempty) snd load
    distPref <- findSavedDistPref config (distPrefFlag flags)
    let setupScriptOptions = defaultSetupScriptOptions { useDistPref = distPref }

    let command' = command { commandName = T.unpack . T.replace "v1-" "" . T.pack . commandName $ command }

    setupWrapper verbosity' setupScriptOptions Nothing
                 command' (const flags) (const extraArgs)

--

class HasVerbosity a where 
    verbosity :: a -> Verbosity

instance HasVerbosity (Setup.Flag Verbosity) where
    verbosity = Setup.fromFlagOrDefault normal

instance (HasVerbosity a) => HasVerbosity (a, b) where
    verbosity (a, _) = verbosity a

instance (HasVerbosity b) => HasVerbosity (a, b, c) where
    verbosity (_ , b, _) = verbosity b

instance (HasVerbosity a) => HasVerbosity (a, b, c, d) where
    verbosity (a, _, _, _) = verbosity a

instance HasVerbosity Setup.BuildFlags where
    verbosity = verbosity . Setup.buildVerbosity

instance HasVerbosity Setup.ConfigFlags where
    verbosity = verbosity . Setup.configVerbosity

instance HasVerbosity Setup.ReplFlags where
    verbosity = verbosity . Setup.replVerbosity

instance HasVerbosity Client.FreezeFlags where
    verbosity = verbosity . Client.freezeVerbosity

instance HasVerbosity Setup.HaddockFlags where
    verbosity = verbosity . Setup.haddockVerbosity

instance HasVerbosity Client.ExecFlags where
    verbosity = verbosity . Client.execVerbosity

instance HasVerbosity Client.UpdateFlags where
    verbosity = verbosity . Client.updateVerbosity

instance HasVerbosity Setup.CleanFlags where
    verbosity = verbosity . Setup.cleanVerbosity

instance HasVerbosity Client.SDistFlags where
    verbosity = verbosity . Client.sDistVerbosity

instance HasVerbosity Client.SandboxFlags where
    verbosity = verbosity . Client.sandboxVerbosity

instance HasVerbosity Setup.DoctestFlags where
    verbosity = verbosity . Setup.doctestVerbosity

--

deprecationNote :: String -> String
deprecationNote cmd = wrapText $
    "The " ++ cmd ++ " command is a part of the legacy v1 style of cabal usage.\n\n" ++

    "Please switch to using either the new project style and the new-" ++ cmd ++ 
    " command or the legacy v1-" ++ cmd ++ " alias as new-style projects will" ++
    " become the default in the next version of cabal-install. Please file a" ++
    " bug if you cannot replicate a working v1- use case with the new-style commands.\n\n" ++

    "For more information, see: https://wiki.haskell.org/Cabal/NewBuild\n"

legacyNote :: String -> String
legacyNote cmd = wrapText $
    "The v1-" ++ cmd ++ " command is a part of the legacy v1 style of cabal usage.\n\n" ++

    "It is a legacy feature and will be removed in a future release of cabal-install." ++
    " Please file a bug if you cannot replicate a working v1- use case with the new-style" ++
    " commands.\n\n" ++

    "For more information, see: https://wiki.haskell.org/Cabal/NewBuild\n"

toLegacyCmd :: (Bool -> CommandSpec (globals -> IO action)) -> [CommandSpec (globals -> IO action)]
toLegacyCmd mkSpec = [toDeprecated (mkSpec True), toLegacy (mkSpec False)]
    where
        legacyMsg = T.unpack . T.replace "v1-" "" . T.pack

        toLegacy (CommandSpec origUi@CommandUI{..} action type') = CommandSpec legUi action type'
            where
                legUi = origUi
                    { commandName = "v1-" ++ commandName
                    , commandNotes = Just $ \pname -> case commandNotes of
                        Just notes -> notes pname ++ "\n" ++ legacyNote commandName
                        Nothing -> legacyNote commandName
                    }

        toDeprecated (CommandSpec origUi@CommandUI{..} action type') = CommandSpec depUi action type'
            where
                depUi = origUi
                    { commandName = legacyMsg commandName
                    , commandUsage = legacyMsg . commandUsage
                    , commandDescription = (legacyMsg .) <$> commandDescription
                    , commandNotes = Just $ \pname -> case commandNotes of
                        Just notes -> legacyMsg (notes pname) ++ "\n" ++ deprecationNote commandName
                        Nothing -> deprecationNote commandName
                    }

legacyCmd :: (HasVerbosity flags) => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
legacyCmd ui action = toLegacyCmd (regularCmd ui action)

legacyWrapperCmd :: Monoid flags => CommandUI flags -> (flags -> Setup.Flag Verbosity) -> (flags -> Setup.Flag String) -> [CommandSpec (Client.GlobalFlags -> IO ())]
legacyWrapperCmd ui verbosity' distPref = toLegacyCmd (wrapperCmd ui verbosity' distPref)

newCmd :: CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
newCmd origUi@CommandUI{..} action = [cmd v2Ui, cmd origUi]
    where
        cmd ui = CommandSpec ui (flip commandAddAction action) NormalCommand
        v2Msg = T.unpack . T.replace "new-" "v2-" . T.pack
        v2Ui = origUi 
            { commandName = v2Msg commandName
            , commandUsage = v2Msg . commandUsage
            , commandDescription = (v2Msg .) <$> commandDescription
            , commandNotes = (v2Msg .) <$> commandDescription
            }
