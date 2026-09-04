import Test.Cabal.Prelude

main :: IO ()
main =
  cabalTest $ do
    skipUnlessGhcVersion ">= 8.8"
    -- GHC >= 9.4 ships a clang-based mingw toolchain, so require it on Windows.
    when isWindows $ skipUnlessGhcVersion ">= 9.4.1"
    env <- getTestEnv
    let pwd = testCurrentDir env
        customCC =
          pwd ++ "/custom-cc" ++ if isWindows then "-clang.bat" else ""
    recordMode DoNotRecord $ cabal "v2-build" ["--with-gcc=" ++ customCC]
