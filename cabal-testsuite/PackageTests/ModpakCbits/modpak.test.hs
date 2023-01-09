import Distribution.Simple.LocalBuildInfo
import Test.Cabal.Prelude

-- Test if modpak files are installed
main = setupAndCabalTest . recordMode DoNotRecord $ do
  withPackageDb $ do
    setup "configure" ["--enable-library-profiling", "--enable-profiling"]
    setup "build" []
    generateModpakFilesToBuildDir
    setup "copy" []

    lbi <- getLocalBuildInfoM
    let installedLibPath = libdir $ absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

    shouldExist $ installedLibPath </> "Lib.o_modpak"
    shouldExist $ installedLibPath </> "Lib.p_o_modpak"
    shouldExist $ installedLibPath </> "Lib.dyn_o_modpak"

generateModpakFilesToBuildDir :: TestM ()
generateModpakFilesToBuildDir = do
  -- modpak files can be generated optionally by ghc plugins or other tools placed next to .hi files
  dist_dir <- fmap testDistDir getTestEnv
  liftIO $ do
    writeFile (dist_dir </> "build/Lib.o_modpak") ""
    writeFile (dist_dir </> "build/Lib.p_o_modpak") ""
    writeFile (dist_dir </> "build/Lib.dyn_o_modpak") ""
