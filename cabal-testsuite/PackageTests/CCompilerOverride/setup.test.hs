import Test.Cabal.Prelude

-- Test that all the respective defines -DNOERROR... specified in various ways
-- all end up routed to the C compiler. Otherwise the C file we depend on will
-- not compile.
main = setupAndCabalTest $ do
  skipUnlessGhcVersion ">= 8.8"
  isWin <- isWindows
  env   <- getTestEnv
  let pwd      = testCurrentDir env
      customCC = pwd ++ "/custom-cc" ++ if isWin then ".bat" else ""

  setup "configure"
    [ "--ghc-option=-DNOERROR1"
    , "--ghc-option=-optc=-DNOERROR2"
    , "--ghc-option=-optP=-DNOERROR3"
    , "--with-gcc=" ++ customCC
    ]
  setup "build" ["-v2"]
