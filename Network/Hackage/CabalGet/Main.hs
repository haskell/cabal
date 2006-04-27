-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalGet.Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-get front-end.
-----------------------------------------------------------------------------
module Network.Hackage.CabalGet.Main where

import System.Environment (getArgs)
import Network.Hackage.CabalGet.Types (Action (..))
import Network.Hackage.CabalGet.Setup (parseGlobalArgs, parseInstallArgs)
import Network.Hackage.CabalGet.Configure (mkConfigFlags)

import Network.Hackage.CabalGet.Install (install)
import Network.Hackage.CabalGet.Info (info)
import Network.Hackage.CabalGet.Update (update)
import Network.Hackage.CabalGet.Fetch (fetch)
import Network.Hackage.CabalGet.Clean (clean)
import Network.Hackage.CabalGet.BuildDep (buildDep)


main :: IO ()
main = do args <- getArgs
          (action, flags, args) <- parseGlobalArgs args
          config <- mkConfigFlags flags
          case action of
            InstallCmd
                -> do (globalArgs, pkgs) <- parseInstallArgs args
                      install config globalArgs pkgs
            BuildDepCmd
                -> do (globalArgs, pkgs) <- parseInstallArgs args
                      buildDep config globalArgs pkgs
            InfoCmd
                -> do (globalArgs, pkgs) <- parseInstallArgs args
                      info config globalArgs pkgs
            UpdateCmd
                -> update config
            CleanCmd
                -> clean config
            FetchCmd
                -> fetch config args
            _ -> putStrLn "Unhandled command."

