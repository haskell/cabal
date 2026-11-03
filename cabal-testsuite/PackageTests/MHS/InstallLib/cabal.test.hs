import Test.Cabal.Prelude

import System.Directory (listDirectory)

-- cabal install --lib with MicroHs, if it is installed: the library, with its
-- C source and its header, goes where MicroCabal installs packages, from where
-- mhs compiles a program that uses it, without cabal.
main = cabalTest $ do
    skipUnless "no mhs" =<< isAvailableProgram mhsProgram
    env <- getTestEnv
    -- Where MicroCabal installs: $CABALDIR, set here to a directory of the
    -- test, as the home directory that the test sets is not honoured on Windows.
    let userDir = takeDirectory (testCabalDir env) </> ".mcabal"
    recordMode DoNotRecord $ withEnv [("CABALDIR", Just userDir)] $ do
        cabal' "v2-install" ["--lib"] >>= assertOutputContains "Installed mhs-installlib-1.0"
        [versionDir] <- liftIO (listDirectory userDir)
        let exe = testWorkDir env </> "prog"
        _ <- runProgramM mhsProgram ["-a" ++ userDir </> versionDir, "prog/Prog.hs", "-o" ++ exe] Nothing
        runM exe [] Nothing >>= assertOutputContains "triple 14 = 42"
