import Test.Cabal.Prelude

-- Test that pkgconfig-depends cflags are propagated to the C compiler
-- when compiling the c-sources of a foreign-library (see #11297).
main = cabalTest $ do
  when isWindows $ skip "pkg-config shim requires sh"
  cdir <- testCurrentDir <$> getTestEnv
  cabal "v2-build" ["--extra-prog-path=" ++ cdir]
