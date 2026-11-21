import Test.Cabal.Prelude

-- A 'build-type: Configure' script must be told which compiler Cabal is
-- configuring the package with (GHC's own libraries run it to generate
-- sources). cabal-install passes the compiler as a program path override,
-- so the runner has to recover it from the program db: the script checks
-- that --with-compiler / --with-hc-pkg are executable paths and that GHC
-- and GHC_PKG are exported.
main = do
  skipIfWindows "relies on a POSIX shell script"
  cabalTest $ cabal "v2-build" []
