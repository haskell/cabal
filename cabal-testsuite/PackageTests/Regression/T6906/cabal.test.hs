import Test.Cabal.Prelude

main = cabalTest $ do
   cabalG' ["--config=cabal.config"] "v2-install" ["-v3"]
   assertOutputContains "File with the inputs used to compute the package hash:"
   assertOutputContains "extra-lib-dirs: bar"
   expectBroken 6906 $ assertOutputDoesNotContain "extra-lib-dirs: bar bar"
   assertOutputContains "extra-include-dirs: foo"
   expectBroken 6906 $ assertOutputDoesNotContain "extra-include-dirs: foo foo"
