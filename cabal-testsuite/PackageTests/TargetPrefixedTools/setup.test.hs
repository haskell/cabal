import Test.Cabal.Prelude

import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory (copyFile, createDirectoryIfMissing, findExecutable)

-- A cross-compiler's ghc and its companion tools share a target prefix
-- (wasm32-unknown-wasi-ghc, wasm32-unknown-wasi-ghc-pkg). Given such a ghc,
-- Cabal has to pick the prefixed ghc-pkg next to it rather than a bare one.
--
-- The fixture is a copy of the ghc and ghc-pkg wrapper scripts under a
-- made-up prefix; a copy, not a symlink, so that the canonical path is the
-- prefixed one and no versioned tool of the real installation is found first.
main = do
  skipIfWindows "POSIX wrapper scripts"
  setupTest . recordMode DoNotRecord $ do
    env <- getTestEnv
    let bin = testTmpDir env </> "prefixed-bin"
    [ghc, ghcPkg] <- forM ["ghc", "ghc-pkg"] $ \tool -> do
        mpath <- liftIO (findExecutable tool)
        case mpath of
            Nothing -> skip ("no " ++ tool ++ " on PATH") >> return ""
            Just path -> return path
    isScript <- liftIO $ ("#!" `isPrefixOf`) <$> readFile ghc
    skipUnless "ghc is not a wrapper script (a copy would not find its libdir)" isScript
    liftIO $ do
        createDirectoryIfMissing True bin
        copyFile ghc (bin </> "foo-bar-ghc")
        copyFile ghcPkg (bin </> "foo-bar-ghc-pkg")
    r <- setup' "configure" ["-v2", "--with-compiler=" ++ bin </> "foo-bar-ghc"]
    -- "Using ghc-pkg version X found on system at:" and the path follow on
    -- separate (wrapped) lines, so check the two halves separately.
    assertOutputContains "Using ghc-pkg version" r
    assertOutputContains (bin </> "foo-bar-ghc-pkg") r
