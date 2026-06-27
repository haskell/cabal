import Test.Cabal.Prelude
import Distribution.System ( buildPlatform )
import Distribution.Simple.BuildPaths ( exeExtension )

-- Test that SetupHooks can set the programPrefix (#11168).
main = setupTest $ do
  env <- getTestEnv
  withPackageDb $ recordMode DoNotRecord $ do
    setup "configure" []
    setup "build" []
    setup "copy" []
    let binDir = testPrefixDir env </> "bin"
        exeExt = exeExtension buildPlatform
    shouldExist    (binDir </> "pre-myexe" <.> exeExt)
    shouldNotExist (binDir </> "myexe"     <.> exeExt)
