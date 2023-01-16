import Distribution.Simple.LocalBuildInfo
import Test.Cabal.Prelude
import System.Directory
import System.FilePath

-- Test if extra-compilation-artifacts are installed
main = setupAndCabalTest . recordMode DoNotRecord $ do
  withPackageDb $ do
    setup "configure" []
    setup "build" []
    generateExtraCompArtifactsToBuildDir
    setup "copy" []

    lbi <- getLocalBuildInfoM
    let installedLibPath = libdir $ absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

    shouldExist $ installedLibPath </> "extra-compilation-artifacts" </> "ghc-plugin-X" </> "data-dir" </> "content-A.txt"
    shouldExist $ installedLibPath </> "extra-compilation-artifacts" </> "ghc-plugin-X" </> "data-dir" </> "content-B.txt"
    shouldExist $ installedLibPath </> "extra-compilation-artifacts" </> "ghc-plugin-Y" </> "content-Y.txt"

generateExtraCompArtifactsToBuildDir :: TestM ()
generateExtraCompArtifactsToBuildDir = do
  -- extra compilation artifacts can be generated optionally by ghc plugins
  dist_dir <- fmap testDistDir getTestEnv
  let genArtifact fname = liftIO $ do
        let dst = dist_dir </> "build" </> "extra-compilation-artifacts" </> fname
        createDirectoryIfMissing True (takeDirectory dst)
        writeFile dst ""
  genArtifact $ "ghc-plugin-X" </> "data-dir" </> "content-A.txt"
  genArtifact $ "ghc-plugin-X" </> "data-dir" </> "content-B.txt"
  genArtifact $ "ghc-plugin-Y" </> "content-Y.txt"

