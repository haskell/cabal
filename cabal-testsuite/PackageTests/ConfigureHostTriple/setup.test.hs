import Test.Cabal.Prelude
import Test.Cabal.Script (runghc)
import Control.Monad.IO.Class
import Data.List (isInfixOf)

-- When cross-compiling, Cabal passes a --host= flag to configure scripts.
-- The value should be the original GNU triple (e.g. x86_64-w64-mingw32),
-- not Cabal's canonical platform string (e.g. x86_64-windows).
--
-- This test uses a fake GHC wrapper that reports "x86_64-w64-mingw32"
-- as its Target platform (simulating a cross-compiler), then checks
-- that the configure script receives --host=x86_64-w64-mingw32
-- rather than the mangled --host=x86_64-windows.
main = do
    skipIfWindows "uses sh script as fake ghc"
    setupTest $ recordMode DoNotRecord $ do
        env <- getTestEnv
        let cwd = testCurrentDir env
            fakeGhc = cwd </> "scripts" </> "fake-ghc.sh"
        -- Run Setup.hs configure with our fake cross-compiler.
        -- We call runghc ourselves to bypass the test framework's
        -- --with-ghc which would override ours.
        _ <- liftIO $ runghc
            (testScriptEnv env)
            (Just $ testTmpDir env)
            (testEnvironment env)
            ("." </> "Setup.hs")
            [ "configure"
            , "--distdir", testDistDir env
            , "--with-compiler", fakeGhc
            ]
        -- The configure script writes its arguments to configure-args.txt
        -- in its working directory (dist/build/).
        let argsFile = testDistDir env </> "build" </> "configure-args.txt"
        args <- liftIO $ readFile argsFile
        -- The configure script should receive --host=x86_64-w64-mingw32
        -- (the original GNU triple), not --host=x86_64-windows.
        unless ("--host=x86_64-w64-mingw32" `isInfixOf` args) $
            error $ unlines
                [ "Expected --host=x86_64-w64-mingw32 in configure arguments"
                , "but got: " ++ args
                ]
