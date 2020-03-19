module Main where

import Criterion.Main                         (bench, defaultMain, env, whnf)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain
    [ env (BS.readFile "Cabal/Cabal.cabal") $ \bs ->
      bench "Cabal" $ whnf parseGenericPackageDescriptionMaybe bs
    , env (BS.readFile "cabal-benchmarks/cabal-benchmarks.cabal") $ \bs ->
      bench "cabal-benchmarks" $ whnf parseGenericPackageDescriptionMaybe bs
    ]
