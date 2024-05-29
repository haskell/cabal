import Test.Cabal.Prelude

-- Test that all the respective defines -DNOERROR... specified in various ways
-- all end up routed to the C compiler. Otherwise the C file we depend on will
-- not compile.
--
-- This has been largely gutted, as ghc 9.10 no longer passes through most
-- of the defines we were testing; see
-- https://gitlab.haskell.org/ghc/ghc/-/commit/8ff3134ed4aa323b0199ad683f72165e51a59ab6
main = setupAndCabalTest $ do
  skipUnlessGhcVersion ">= 8.8"
  isWin <- isWindows
  ghc94 <- isGhcVersion ">= 9.4.1"
  env   <- getTestEnv
  let pwd      = testCurrentDir env
      win_suffix = if ghc94 then "-clang.bat" else ".bat"
      customCC =
        pwd ++ "/custom-cc" ++ if isWin then win_suffix else ""

  setup "configure"
    [ "--ghc-option=-optc=-DNOERROR2"
    , "--with-gcc=" ++ customCC
    ]
  setup "build" ["-v2"]
