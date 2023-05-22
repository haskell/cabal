import Test.Cabal.Prelude
-- Test that we can resolve a module name ambiguity when reexporting
-- by explicitly specifying what package we want.
main = cabalTest $ withDirectory "multiple-cabal-files" $
    fails $ cabal "v2-build" []
