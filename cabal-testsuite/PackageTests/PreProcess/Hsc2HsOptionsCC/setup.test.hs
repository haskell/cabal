import Test.Cabal.Prelude
import Distribution.Pretty (prettyShow)
import Data.Maybe (isJust)
import System.Directory (findExecutable)

-- Check that preprocessors (hsc2hs) are run
main = setupAndCabalTest $ do
    -- we need "g++"
    hasGxx <- liftIO $ fmap isJust $ findExecutable "g++"
    skipUnless "g++" hasGxx

    -- Figure out how recent GHC we need
    -- https://github.com/msys2/MINGW-packages/issues/3531
    skipIfWindows

    -- we need recent enough hsc2hs
    -- hsc2hs commit 9671202c11f7fe98e5b96d379532b6f691dc46dd
    -- Fix when using g++ as C compiler. Patch from elaforge. Fixes ghc #7232
    p <- requireProgramM hsc2hsProgram
    case programVersion p of
        Nothing                       -> skip "Unknown hsc2hs version"
        Just v | v < mkVersion [0,68] -> skip $ "hsc2hs version: " ++ prettyShow v ++ " < 0.68"
        _                             -> return ()

    -- Actual test
    setup_build []
    r <- runExe' "my-executable" []
    assertOutputContains "Is not C, is C++" r
