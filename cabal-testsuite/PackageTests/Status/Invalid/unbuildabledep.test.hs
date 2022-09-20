{-# LANGUAGE OverloadedStrings #-}
import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ withRepo "repo" $ do
  -- no build plan available
  r <- fails $ cabal' "status" ["--output-format=json", "--target", "src/Main.hs"]
  assertOutputContains "Could not resolve dependencies" r
  -- TODO: should this actually work?
  r <- fails $ cabal' "status" ["--output-format=json", "--compiler"]
  assertOutputContains "Could not resolve dependencies" r
