-- module to download & test build all cabal packages in a given repo

module Main where

import System.Environment (getArgs)
import System.Cmd(system)
import Network.Hackage.CabalInstall.Config (getKnownPackages)
import Network.Hackage.CabalInstall.Configure (mkConfigFlags)
import Network.Hackage.CabalInstall.Setup (parseGlobalArgs)
import Network.Hackage.CabalInstall.Types (PkgInfo(..), ConfigFlags(..))
import Distribution.Package(PackageIdentifier(pkgName))

getPkg :: PkgInfo -> IO ()
getPkg p = do
  system ("cabal-install --user-install install " ++ (pkgName $ infoId p))
  return ()

main = do
  args <- getArgs
  (action, flags, args) <- parseGlobalArgs args
  config <- mkConfigFlags flags
  pkgs <- getKnownPackages config
  let pkgsShort = take 4 pkgs
  mapM (\p -> putStrLn ("pkg: " ++ (show (pkgName $ infoId p)))) pkgsShort
  mapM getPkg pkgsShort

  
  return ()
