-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
-----------------------------------------------------------------------------

module Main where

import Hackage.Types            (Action (..), Option(..))
import Hackage.Setup            (parseGlobalArgs, parsePackageArgs, configFromOptions)
import Hackage.Config           (defaultConfigFile, loadConfig)
import Hackage.List             (list)
import Hackage.Install          (install)
import Hackage.Info             (info)
import Hackage.Update           (update)
import Hackage.Fetch            (fetch)
import Hackage.Clean            (clean)

import System.Environment       (getArgs)

-- | Entry point
--
main :: IO ()
main = do
    rawArgs    <- getArgs
    (action, flags, args) <- parseGlobalArgs rawArgs
    configFile <- case [f | OptConfigFile f <- flags] of
                        [] -> defaultConfigFile
                        fs -> return (last fs)

    conf0      <- loadConfig configFile

    let config = configFromOptions conf0 flags

        runCmd f = do (globalArgs, pkgs) <- parsePackageArgs action args
                      f config globalArgs pkgs

    case action of
        InstallCmd  -> runCmd install
        InfoCmd     -> runCmd info
        ListCmd     -> list   config args
        UpdateCmd   -> update config
        CleanCmd    -> clean  config
        FetchCmd    -> runCmd fetch
        _           -> error "Unhandled command."

