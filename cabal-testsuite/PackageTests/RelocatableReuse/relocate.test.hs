import Test.Cabal.Prelude
import Test.Cabal.Monad (testSourceCopyDir, testVerbosity)

import Control.Monad.Trans.Reader (withReaderT)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import System.Directory (copyFile, createDirectoryIfMissing)

-- Copying the whole project (sources + dist tree) to a different absolute
-- location and rebuilding with @--enable-relocatable@ must reuse the existing
-- build artifacts rather than recompiling.
--
-- The dist tree is kept INSIDE the project (a relative @--builddir@) so that
-- relocating the whole tree preserves the relationship between the sources and
-- the build directory — which is what a real relocation looks like (the
-- default @dist-newstyle@ lives inside the project). The config file monitor
-- keys off paths relative to the build dir, so this relationship being
-- preserved is what makes the relocated build reuse the artifacts.
main = do
  skipIfWindows "does not support relocatable builds"
  cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let v = testVerbosity env
        root = testCurrentDir env
        origSrc = testSourceCopyDir env </> "orig"
        relocSrc = testSourceCopyDir env </> "relocated"
        -- A relative --builddir places the dist tree inside the project dir.
        relocBuild = ["--enable-relocatable", "--builddir=dist-reloc"]
        copyPkg from to = liftIO $ do
          createDirectoryIfMissing True to
          copyFile (from </> "relocatable-reuse.cabal") (to </> "relocatable-reuse.cabal")
          copyFile (from </> "cabal.project") (to </> "cabal.project")
          copyDirectoryRecursive v (from </> "src") (to </> "src")
          copyDirectoryRecursive v (from </> "app") (to </> "app")
          copyDirectoryRecursive v (from </> "data") (to </> "data")

    -- Build the package in the 'orig' subdirectory; dist-reloc is created
    -- inside it.
    copyPkg root origSrc
    withReaderT (\e -> e{testRelativeCurrentDir = "orig"}) $
      cabal "build" relocBuild

    -- Relocate: copy the whole tree (sources + the inner dist-reloc) to a
    -- different absolute location.
    liftIO $ copyDirectoryRecursive v origSrc relocSrc

    -- Building in the relocated location must find everything up to date.
    withReaderT (\e -> e{testRelativeCurrentDir = "relocated"}) $ do
      r <- cabal' "build" relocBuild
      assertOutputContains "Up to date" r
      assertOutputDoesNotContain "Compiling" r
