import Test.Cabal.Prelude

-- Test for issue #8900: `ghc-options` order when combining multiple
-- configuration files. The `program-options` stanza in `cabal.project.local`
-- must be applied after `package <name> ghc-options` in `cabal.project`, so
-- that `-Wwarn` can override `-Werror`.
main = cabalTest $ recordMode DoNotRecord $ do
  skipUnlessGhcVersion ">= 8.8"
  r <- cabal' "v2-build" ["all"]
  assertOutputContains "warning:" r
