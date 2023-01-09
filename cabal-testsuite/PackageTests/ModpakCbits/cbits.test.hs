import Distribution.Simple.LocalBuildInfo
import Test.Cabal.Prelude

-- Test that cbits archive is built and installed for c, cxx, cmm, asm sources
main = setupAndCabalTest . recordMode DoNotRecord $ do
  withPackageDb $ do
    setup "configure" ["--enable-library-profiling", "--enable-profiling"]
    setup "build" []
    setup "copy" []

    lbi <- getLocalBuildInfoM
    let installedLibPath = libdir $ absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

    shouldExist $ installedLibPath </> "libHSlib-0.1.0.0.o_cbits.a"
    shouldExist $ installedLibPath </> "libHSlib-0.1.0.0.p_o_cbits.a"
    shouldExist $ installedLibPath </> "libHSlib-0.1.0.0.dyn_o_cbits.a"
