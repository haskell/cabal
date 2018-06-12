{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.Client.CmdLegacy (legacyCmd) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Distribution.Client.Setup as Client
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Command
import Distribution.Simple.Utils
    ( warn )
import Distribution.Verbosity 
    ( Verbosity, normal )

import qualified Data.Text as T

-- Duplicated code (it's identical to Main.regularCmd), but makes things cleaner
-- and lets me keep how this happens a dirty little secret.
makeCmd :: CommandUI flags -> (flags -> [String] -> action) -> CommandSpec action
makeCmd ui action = CommandSpec ui ((flip commandAddAction) action) NormalCommand

deprecationNote :: String -> String
deprecationNote cmd =
    "The " ++ cmd ++ " command is a part of the legacy v1 style of cabal usage.\n\n" ++
                          
    "Please switch to using either the new project style or the legacy v1-" ++ cmd ++ "\n" ++
    "alias as new-style projects will become the default in the next version of\n" ++
    "cabal-install. Please file a bug if you cannot replicate a working v1- use\n" ++
    "case with the new-style commands.\n"

legacyNote :: String -> String
legacyNote cmd =
    "The v1-" ++ cmd ++ " command is a part of the legacy v1 style of cabal usage.\n\n" ++

    "It is a legacy feature and will be removed in a future release of cabal-install.\n" ++
    "Please file a bug if you cannot replicate a working v1- use case with the new-style\n" ++
    "commands.\n"

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

--

legacyCmd :: (HasVerbosity flags) => CommandUI flags -> (flags -> [String] -> globals -> IO action) -> [CommandSpec (globals -> IO action)]
legacyCmd origUi@CommandUI{..} action = [makeCmd legUi action, makeCmd depUi depAction]
    where
        legacyMsg = T.unpack . T.replace "v1-" "" . T.pack

        depNote = deprecationNote commandName
        legNote = legacyNote commandName

        depAction flags extra globals = warn (verbosity flags) (depNote ++ "\n") >> action flags extra globals

        legUi = origUi 
            { commandName = "v1-" ++ commandName
            , commandNotes = Just $ \pname -> case commandNotes of
                Just notes -> notes pname ++ "\n" ++ legNote
                Nothing -> legNote
            }
        
        depUi = origUi
            { commandName = legacyMsg commandName
            , commandUsage = legacyMsg . commandUsage
            , commandDescription = (legacyMsg .) <$> commandDescription
            , commandNotes = Just $ \pname -> case commandNotes of
                Just notes -> legacyMsg (notes pname) ++ "\n" ++ depNote
                Nothing -> depNote
            }
