import System.Directory
import Test.Cabal.Prelude

-- Test that invalid unicode in pkg-config output doesn't trip up cabal very much
main = cabalTest $ do
  when isWindows $ do
        sh <- fmap takeDirectory <$> liftIO (findExecutable "sh")
        case sh of
            Nothing -> skip "no sh"
            Just sh' -> do
                let sh'' = concatMap (\c -> case c of
                            '\\' -> "\\\\\\\\"
                            x -> [x]) sh'
                void $ shell "sed" [ "-i", "-e", "s/FINDSH/" <> sh'' <> "/g", "pkg-config.shim"]
  cdir <- testCurrentDir `fmap` getTestEnv
  res <- cabal' "v2-build" ["--extra-prog-path="++cdir, "-v2"]
  assertOutputContains "Some pkg-config packages have names containing invalid unicode: or" res
