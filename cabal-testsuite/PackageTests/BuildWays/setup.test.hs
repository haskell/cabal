import Test.Cabal.Prelude

opts = ["--enable-shared", "--enable-library-vanilla", "--enable-library-profiling"]

-- See #10418
main = setupTest $ recordMode DoNotRecord $ withPackageDb $ do
    skipIfNoSharedLibraries
    skipIfNoProfiledLibraries
    withDirectory "p" $ setup_install opts
    withDirectory "q" $ setup_install opts
