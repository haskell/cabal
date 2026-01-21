import Test.Cabal.Prelude
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, getPermissions, setPermissions, Permissions(..))

main :: IO ()
main = do
    skipIfWindows "relies on a POSIX shell script"
    cabalTest $ do
        env <- getTestEnv
        let fakeDir = testTmpDir env </> "fake-ghc-pkg-bin"
        liftIO $ createDirectoryIfMissing True fakeDir
        let fakeGhcPkg = fakeDir </> "ghc-pkg"
            fakeGhc    = fakeDir </> "ghc"
        realGhcPkg <- programPathM ghcPkgProgram
        realGhc    <- programPathM ghcProgram

        liftIO $ do
            writeFile fakeGhcPkg "#!/bin/sh\n\necho \"fake ghc-pkg\" >&2\nexit 1\n"
            perms <- getPermissions fakeGhcPkg
            setPermissions fakeGhcPkg perms { executable = True }
            writeFile fakeGhc $ "#!/bin/sh\nexec \"" ++ realGhc ++ "\" \"$@\"\n"
            ghcPerms <- getPermissions fakeGhc
            setPermissions fakeGhc ghcPerms { executable = True }

        -- Passing -w makes GHC look for ghc-pkg in the fake directory alongside the ghc wrapper.
        -- The wrapper fails.
        fails $ cabal "v2-build" ["-w", fakeGhc, "all"]

        -- Adding --with-hc-pkg must override that lookup and use the real
        -- tool even though the configured compiler still lives in fakeDir.
        cabal "v2-build" ["-w", fakeGhc, "--with-hc-pkg", realGhcPkg, "all"]
