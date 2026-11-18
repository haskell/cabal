import Test.Cabal.Prelude

-- The configure script fails if it is invoked with a doubled leading slash.
-- Autoconf scripts derive $srcdir from their own path, and tools that compare
-- paths as strings (nixpkgs' cc-wrapper) do not recognise //build as /build.
main = setupTest . recordMode DoNotRecord $ setup "configure" []
