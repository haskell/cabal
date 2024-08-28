import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv
  recordMode DoNotRecord $ do
    let
      installdir = testPrefixDir env </> "bin"
      commonOpts v = ["--ghc-options=-DTEST" ++ show v, "--overwrite-policy=always", "--installdir=" ++ installdir]
      installWithTgt tgt v = do
        cabal "install" (tgt:commonOpts v)
        runInstalledExe' "my-exe" []
          >>= assertOutputContains ("hi" ++ show v)

      installExternalWithTgt tgt v = withRepo "repo" (installWithTgt tgt v)


    cabal "install" (commonOpts 1) -- no target
    runInstalledExe' "my-exe" []
      >>= assertOutputContains "hi1"

    installWithTgt "t7297-89097236a" 2
    installWithTgt "exe:my-exe" 3
    installWithTgt "my-exe" 4
    installWithTgt "all" 5
    installWithTgt "all:exes" 6

    -- And test it works when installing from an external repo (think Hackage)
    installExternalWithTgt "external-lib" 2
