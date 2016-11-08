import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    fails $ setup "configure" []
    setup "configure" ["--allow-older"]
    fails $ setup "configure" ["--allow-older=baz,quux"]
    setup "configure" ["--allow-older=base", "--allow-older=baz,quux"]
    setup "configure" ["--allow-older=bar", "--allow-older=base,baz"
                      ,"--allow-older=quux"]
    fails $ setup "configure" ["--enable-tests"]
    setup "configure" ["--enable-tests", "--allow-older"]
    fails $ setup "configure" ["--enable-benchmarks"]
    setup "configure" ["--enable-benchmarks", "--allow-older"]
    fails $ setup "configure" ["--enable-benchmarks", "--enable-tests"]
    setup "configure" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-older"]
    fails $ setup "configure" ["--allow-older=Foo:base"]
    fails $ setup "configure" ["--allow-older=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    setup "configure" ["--allow-older=AllowOlder:base"]
    setup "configure" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"]
    setup "configure" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
