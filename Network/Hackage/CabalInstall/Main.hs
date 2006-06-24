-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Main where

import System.Environment (getArgs)
import Network.Hackage.CabalInstall.Types (Action (..))
import Network.Hackage.CabalInstall.Setup (parseGlobalArgs, parseInstallArgs)
import Network.Hackage.CabalInstall.Configure (mkConfigFlags)

import Network.Hackage.CabalInstall.List (list)
import Network.Hackage.CabalInstall.Install (install)
import Network.Hackage.CabalInstall.Info (info)
import Network.Hackage.CabalInstall.Update (update)
import Network.Hackage.CabalInstall.Fetch (fetch)
import Network.Hackage.CabalInstall.Clean (clean)
import Network.Hackage.CabalInstall.BuildDep (buildDep)


main :: IO ()
main = do args <- getArgs
          (action, flags, args) <- parseGlobalArgs args
          config <- mkConfigFlags flags
          let runCmd f = do (globalArgs, pkgs) <- parseInstallArgs args
                            f config globalArgs pkgs
          case action of
            InstallCmd  -> runCmd install
            BuildDepCmd -> runCmd buildDep
            InfoCmd     -> runCmd info
            ListCmd     -> list config args
            UpdateCmd   -> update config
            CleanCmd    -> clean config
            FetchCmd    -> fetch config args
            _           -> putStrLn "Unhandled command."
