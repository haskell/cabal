import Test.Cabal.Prelude

main = cabalTest $ do
   res <- cabalG' ["--config=cabal.config"] "v2-install" ["-v3"]
   assertOutputContains "creating file with the inputs used to compute the package hash:" res
   assertOutputContains "extra-lib-dirs: bar" res
   assertOutputDoesNotContain "extra-lib-dirs: bar bar" res
   assertOutputContains "extra-include-dirs: foo" res
   assertOutputDoesNotContain "extra-include-dirs: foo foo" res
