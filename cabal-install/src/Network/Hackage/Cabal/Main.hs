-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.Cabal.Main
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Entry point to the default cabal-install front-end.
-----------------------------------------------------------------------------
module Network.Hackage.Cabal.Main where

import Data.List (isSuffixOf)
import System.Environment (getArgs)
import Network.Hackage.Cabal.Types (Action (..), Option(..))
import Network.Hackage.Cabal.Setup (parseGlobalArgs, parsePackageArgs, configFromOptions)
import Network.Hackage.Cabal.Config (defaultConfigFile, loadConfig)

import Network.Hackage.Cabal.List (list)
import Network.Hackage.Cabal.Install (install)
import Network.Hackage.Cabal.Info (info)
import Network.Hackage.Cabal.Update (update)
import Network.Hackage.Cabal.Fetch (fetch)
import Network.Hackage.Cabal.Clean (clean)


main :: IO ()
main = do args <- getArgs
          (action, flags, args) <- parseGlobalArgs args
          configFile <- case [f | OptConfigFile f <- flags] of
                          [] -> defaultConfigFile
                          fs -> return $ last fs
          conf0  <- loadConfig configFile
          let config = configFromOptions conf0 flags
              runCmd f = do (globalArgs, pkgs) <- parsePackageArgs action args
                            f config globalArgs pkgs
          case action of
            InstallCmd  -> runCmd install
            InfoCmd     -> runCmd info
            ListCmd     -> list config args
            UpdateCmd   -> update config
            CleanCmd    -> clean config
            FetchCmd    -> fetch config args
            _           -> putStrLn "Unhandled command."
