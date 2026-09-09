import Test.Cabal.Prelude

-- A 'build-type: Configure' script must be told which compiler Cabal is
-- configuring the package with (GHC's own libraries run it to generate
-- sources). The script checks that --with-compiler, --with-hc-pkg and HC=
-- are executable paths, that GHC and GHC_PKG are exported, and that HC=
-- comes before the configure-options from cabal.project, which can then
-- override it.
main = do
  skipIfWindows "relies on a POSIX shell script"
  cabalTest $ cabal "v2-build" []
