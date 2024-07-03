import Test.Cabal.Prelude
-- Test building an executable whose main-is is a C source found in the hs-source-dirs.
-- It is a bit counter intuitive that we look for non-haskell sources in
-- `hs-source-dirs`, but that is a behaviour that users rely on (see #10168)
-- and there's no good reason to break it.
main = setupTest $ do
  setup_build []
