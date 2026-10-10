import Test.Cabal.Prelude
import Test.Cabal.Monad (testSourceCopyDir, testVerbosity)

import Control.Monad.Trans.Reader (withReaderT)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import System.Directory (copyFile, createDirectoryIfMissing)

main = do
  skipIfWindows "build-tree relocation is not verified on Windows"
  cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    let v = testVerbosity env
        root = testCurrentDir env
        origSrc = testSourceCopyDir env </> "orig"
        relocSrc = testSourceCopyDir env </> "relocated"
        -- A relative --builddir places the dist tree inside the project dir.
        relocBuild = ["--builddir=dist-reloc"]
        copyPkg from to = liftIO $ do
          createDirectoryIfMissing True to
          copyFile (from </> "relocatable-reuse.cabal") (to </> "relocatable-reuse.cabal")
          copyFile (from </> "cabal.project") (to </> "cabal.project")
          copyDirectoryRecursive v (from </> "src") (to </> "src")
          copyDirectoryRecursive v (from </> "app") (to </> "app")
          copyDirectoryRecursive v (from </> "data") (to </> "data")

    copyPkg root origSrc
    withReaderT (\e -> e{testRelativeCurrentDir = "orig"}) $
      cabal "build" relocBuild

    liftIO $ copyDirectoryRecursive v origSrc relocSrc

    withReaderT (\e -> e{testRelativeCurrentDir = "relocated"}) $ do
      r <- cabal' "build" relocBuild
      assertOutputContains "Up to date" r
      assertOutputDoesNotContain "Compiling" r
