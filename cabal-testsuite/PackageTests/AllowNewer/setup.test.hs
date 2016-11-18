import Test.Cabal.Prelude
-- Test Setup.hs understand --allow-newer
main = setupAndCabalTest $ do
    fails $ setup "configure" []
    setup "configure" ["--allow-newer"]
    fails $ setup "configure" ["--allow-newer=baz,quux"]
    setup "configure" ["--allow-newer=base", "--allow-newer=baz,quux"]
    setup "configure" ["--allow-newer=bar", "--allow-newer=base,baz"
                      ,"--allow-newer=quux"]
    fails $ setup "configure" ["--enable-tests"]
    setup "configure" ["--enable-tests", "--allow-newer"]
    fails $ setup "configure" ["--enable-benchmarks"]
    setup "configure" ["--enable-benchmarks", "--allow-newer"]
    fails $ setup "configure" ["--enable-benchmarks", "--enable-tests"]
    setup "configure" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-newer"]
    fails $ setup "configure" ["--allow-newer=Foo:base"]
    fails $ setup "configure" ["--allow-newer=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    setup "configure" ["--allow-newer=AllowNewer:base"]
    setup "configure" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"]
    setup "configure" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
