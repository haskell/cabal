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
                void $ shell "sed" [ "-i", "-e", "s/FINDSH/" <> sh'' <> "/g", "bin/ghc-7.10.shim", "bin/ghc-pkg-ghc-7.10.shim"]
    (if isWindows
        then withSymlink "bin/ghc-7.10.exe" "ghc.exe" . withSymlink "bin/ghc-7.10.shim" "ghc.shim" . withSymlink "bin/ghc-7.10" "ghc"
        else withSymlink "bin/ghc-7.10" "ghc") $ do
        env <- getTestEnv
        let cwd = testCurrentDir env
        ghc_path <- programPathM ghcProgram
        r <- withEnv [("WITH_GHC", Just ghc_path)]
           . fails $ setup' "configure" ["-w", cwd </> if isWindows then "ghc.exe" else "ghc"]
        assertOutputContains "is version 9999999" r
