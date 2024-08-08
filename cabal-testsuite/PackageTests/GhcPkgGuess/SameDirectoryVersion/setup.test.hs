import Test.Cabal.Prelude
import System.Directory

main = setupAndCabalTest $ do
      when isWindows $ do
        sh <- fmap takeDirectory <$> liftIO (findExecutable "sh")
        case sh of
            Nothing -> skip "no sh"
            Just sh' -> do
                let sh'' = concatMap (\c -> case c of
                            '\\' -> "\\\\\\\\"
                            x -> [x]) sh'
                void $ shell "sed" [ "-i", "-e", "s/FINDSH/" <> sh'' <> "/g", "ghc-7.10.shim", "ghc-pkg-7.10.shim"]
      env <- getTestEnv
      let cwd = testCurrentDir env
      ghc_path <- programPathM ghcProgram
      r <- withEnv [("WITH_GHC", Just ghc_path)]
         . fails $ setup' "configure" ["-w", cwd </> if isWindows then "ghc-7.10.exe" else "ghc-7.10"]
      assertOutputContains "is version 9999999" r
