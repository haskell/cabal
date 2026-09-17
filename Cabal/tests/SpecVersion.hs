module Main (main) where

import Data.Version (versionBranch)
import Distribution.CabalSpecVersion (cabalSpecLatest, cabalSpecToVersionDigits)
import Paths_Cabal qualified as Pkg
import System.Exit (exitFailure)

main :: IO ()
main = do
  let specDigits = cabalSpecToVersionDigits cabalSpecLatest
      libDigits = take 2 (versionBranch Pkg.version)
  if specDigits >= libDigits
    then putStrLn ("OK: cabalSpecLatest " ++ show specDigits ++ " >= Cabal " ++ show libDigits)
    else do
      putStrLn
        ( "FAIL: cabalSpecLatest "
            ++ show specDigits
            ++ " is behind Cabal "
            ++ show libDigits
        )
      exitFailure
