import Test.Cabal.Prelude
-- Test build-tool-depends between two packages
main = cabalTest $ do
    -- main library, and executable should run configure and generate a
    -- buildinfo file.
    cabal' "build" ["test-pkg:test-pkg"] >>=
        assertOutputContains "config.status: creating test-pkg.buildinfo"
    cabal' "v2-build" ["test-pkg:exe"] >>=
        assertOutputContains "config.status: creating test-pkg.buildinfo"
    -- configure values should not be embeddable in the executable.
    cabal' "v2-run"   ["test-pkg:exe"] >>=
        assertOutputContains "Hi-Exe"
    -- configure values should be embeddable in the library.
    cabal' "v2-run"   ["test-pkg:exe-dep"] >>=
        assertOutputContains "Hi-Lib"
    -- sublibs should not run configure
    cabal' "v2-build" ["test-pkg:lib:foo"] >>=
        assertOutputDoesNotContain "config.status: creating test-pkg.buildinfo"
    cabal' "v2-build" ["test-pkg:lib:bar"] >>=
        assertOutputDoesNotContain "config.status: creating test-pkg.buildinfo"

