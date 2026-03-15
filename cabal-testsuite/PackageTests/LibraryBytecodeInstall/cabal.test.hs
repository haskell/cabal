import Test.Cabal.Prelude
import System.Directory (removeFile)
import Control.Monad (mapM_)

-- Test that installed bytecode libraries can be used in GHCi
-- with --repl-options="-fprefer-byte-code"
-- This tests the installation logic: we install pkg-a with bytecode,
-- then start ghci with pkg-b (which depends on pkg-a) and verify
-- that pkg-a is loaded from the installed bytecode library.
main = cabalTest $ defaultRecordMode RecordMarked $ do
    -- Bytecode artifacts are only supported in GHC >= 9.15.0
    skipUnlessGhcVersion ">= 9.15"

    -- Build library-bytecode for pkg-a
    cabal' "v2-build" ["--enable-library-bytecode", "pkg-a"]

    -- Delete native library files (.so and .a) for pkg-a to ensure
    -- we're definitely loading the bytecode library
    env <- getTestEnv
    let distDir = testDistDir env
    -- Find all .so and .a files in pkg-a build directories
    -- Library files can be named libHSpkg-a-*.so or libpkg-a-*.so
    nativeLibs <- liftIO $ matchGlob distDir "**/pkg-a-0.1/**/*.so"
    nativeArchives <- liftIO $ matchGlob distDir "**/pkg-a-0.1/**/*.a"
    assertBool "No native libraries, expected at least one" (not $ null (nativeLibs ++ nativeArchives))

    liftIO $ mapM_ removeFile (nativeLibs ++ nativeArchives)

    -- Start GHCi with pkg-b (which depends on pkg-a) and use -fprefer-byte-code
    -- Even though pkg-a is in the project, with -fprefer-byte-code GHC should
    -- prefer the installed bytecode version if available
    res <- cabalWithStdin "v2-repl" ["--enable-library-bytecode", "--repl-options=-fprefer-byte-code", "pkg-b"] "LibB.libBValue"

    -- Verify that we can execute the function from pkg-a
    -- The value should be 43 (6 * 7 from multiplyNumbers + 1 from addNumbers)
    assertOutputContains "43" res
    -- Does not contain - pkg-a in the build description.
    assertOutputDoesNotContain "- pkg-a" res
    -- Does contain pkg-b (just in case the output changes)
    assertOutputContains "- pkg-b" res

