import Test.Cabal.Prelude
import Distribution.Pretty (prettyShow)
import Data.Maybe (isJust)
import System.Directory (findExecutable)

-- Check that preprocessors (hsc2hs) are run
main = setupAndCabalTest $ do
    -- we need "g++" (or "clang++" in newer Windows)
    ghcVer <- isGhcVersion ">= 9.4"
    cc <- if isWindows
            then
                -- The mingw tools distributed with GHC are not usually on the
                -- path so we specify the path to cc directly.
                   joinPath
                .   (++ ["mingw", "bin", if ghcVer then "clang++.exe" else "g++.exe"])
                .   init
                .   splitPath
                .   resultOutput
                <$> runProgramM ghcProgram ["--print-libdir"] Nothing
            else do
                hasGxx <- liftIO $ fmap isJust $ findExecutable "g++"
                skipUnless "g++" hasGxx
                pure "g++"

    -- we need recent enough hsc2hs
    -- hsc2hs commit 9671202c11f7fe98e5b96d379532b6f691dc46dd
    -- Fix when using g++ as C compiler. Patch from elaforge. Fixes ghc #7232
    -- We also require 0.68.8 so that last --cc is the one that applies.
    p <- requireProgramM hsc2hsProgram
    case programVersion p of
        Nothing                         -> skip "Unknown hsc2hs version"
        Just v | v < mkVersion [0,68,8] -> skip $ "hsc2hs version: " ++ prettyShow v ++ " < 0.68.8"
        _                               -> return ()

    -- Actual test
    setup_build ["--hsc2hs-options=\"--cc=" <> cc <> "\""]
    r <- runExe' "my-executable" []
    assertOutputContains "Is not C, is C++" r
