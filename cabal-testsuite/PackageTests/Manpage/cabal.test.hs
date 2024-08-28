import System.Process
import Distribution.System (OS(Windows,Linux,OSX), buildOS)
import Test.Cabal.Prelude

main = cabalTest $ do
    r <- cabal' "man" ["--raw"]
    assertOutputContains ".B cabal install" r
    assertOutputDoesNotContain ".B cabal manpage" r

    -- The following test of `cabal man` needs `nroff` which is not available under Windows and OSX.
    unless (buildOS == Windows || buildOS ==OSX) $ do

      -- Check that output of `cabal man --raw` can be passed through `nroff -man`
      -- without producing any warnings (which are printed to stderr).
      --
      -- NB: runM is not suitable as it mixes stdout and stderr
      -- r2 <- runM "nroff" ["-man", "/dev/stdin"] $ Just $ resultOutput r
      (ec, _output, errors) <- liftIO $
        readProcessWithExitCode "nroff" ["-man", "/dev/stdin"] $ resultOutput r
      unless (null errors) $
        assertFailure $ unlines
          [ "Error: unexpected warnings produced by `nroff -man`:"
          , errors
          ]
