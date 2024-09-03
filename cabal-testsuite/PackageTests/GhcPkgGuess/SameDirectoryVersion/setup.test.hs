import Test.Cabal.Prelude

main = setupAndCabalTest $ expectBrokenIfWindows 10179 $ do
      env <- getTestEnv
      let cwd = testCurrentDir env
      ghc_path <- programPathM ghcProgram
      r <- withEnv [("WITH_GHC", Just ghc_path)]
         . fails $ setup' "configure" ["-w", cwd </> "ghc-7.10"]
      assertOutputContains "is version 9999999" r
