module Main (main) where

import Data.Version (versionBranch)
import Distribution.CabalSpecVersion (cabalSpecLatest, cabalSpecToVersionDigits)
import Paths_cabal_install_solver qualified as Pkg
import System.Exit (exitFailure)

main :: IO ()
main = do
  let specDigits = cabalSpecToVersionDigits cabalSpecLatest
      libDigits = take 2 (versionBranch Pkg.version)
  if specDigits >= libDigits
    then putStrLn ("OK: cabalSpecLatest " ++ show specDigits ++ " >= cabal-install-solver " ++ show libDigits)
    else do
      putStrLn
        ( "FAIL: cabalSpecLatest "
            ++ show specDigits
            ++ " is behind cabal-install-solver "
            ++ show libDigits
        )
      exitFailure
