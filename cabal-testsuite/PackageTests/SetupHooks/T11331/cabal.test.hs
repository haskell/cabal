import Test.Cabal.Prelude

{- Test for https://github.com/haskell/cabal/issues/11331

This test works by having a package database with two Cabal libraries in it,
a good one (Cabal-good) and a poisoned one (Cabal-bad).

Cabal-good depends on stub == 0.2 and Cabal-bad depends on stub == 0.1.

Moreover, we have a fancy-hooks package in the package database, that has been
built against Cabal-good.

T11331 has the following Cabal file:

  T11331.setup --> fancy-hooks
  T11331.lib   --> stub == 0.1

The test checks that, for the setup component, we correctly use Cabal-good
and not Cabal-bad, as only Cabal-good is compatible with fancy-hooks (correct
ABI hash of Cabal dependency).
-}
main :: IO ()
main = cabalTest $ recordMode DoNotRecord $ withPackageDb $ do
  -- Build good package set 'fancy-hooks -> Cabal-good -> stub-0.2' first.
  withDirectory "stub-0.2" $ do
    setup "configure" []
    setup "build" []
    setup "register" ["--inplace"]
  withDirectory "Cabal-good" $ do
    setup "configure" []
    setup "build" []
    setup "register" ["--inplace"]
  withDirectory "fancy-hooks" $ do
    setup "configure" []
    setup "build" []
    setup "register" ["--inplace"]

  -- Now build bad package set 'Cabal-bad -> stub-0.1'.
  withDirectory "stub-0.1" $ do
    setup "configure" []
    setup "build" []
    setup "register" ["--inplace"]
  withDirectory "Cabal-bad" $ do
    setup "configure" []
    setup "build" []
    setup "register" ["--inplace"]

  -- Now try to build 'T11331' with cabal-install, using the package database
  -- that we carefully crafted above. This tests that cabal-install picks
  --  'Cabal-good' for the setup component of 'T11331', otherwise we will fail.
  dbDir <- testPackageDbDir <$> getTestEnv

  -- This fails because the fake Cabal-good defaultMain doesn't do anything
  -- except print "Chosen GOOD Cabal".
  res <- fails $ cabal' "build" [ "--package-db=" ++ dbDir, "T11331" ]
  assertOutputContains "Chosen GOOD Cabal" res
