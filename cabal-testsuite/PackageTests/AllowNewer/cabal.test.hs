import Test.Cabal.Prelude hiding (cabal)
import qualified Test.Cabal.Prelude as P
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ do
    fails $ cabal "v2-build" []
    cabal "v2-build" ["--allow-newer"]
    fails $ cabal "v2-build" ["--allow-newer=baz,quux"]
    cabal "v2-build" ["--allow-newer=base", "--allow-newer=baz,quux"]
    cabal "v2-build" ["--allow-newer=bar", "--allow-newer=base,baz"
                      ,"--allow-newer=quux"]
    fails $ cabal "v2-build" ["--enable-tests"]
    cabal "v2-build" ["--enable-tests", "--allow-newer"]
    fails $ cabal "v2-build" ["--enable-benchmarks"]
    cabal "v2-build" ["--enable-benchmarks", "--allow-newer"]
    fails $ cabal "v2-build" ["--enable-benchmarks", "--enable-tests"]
    cabal "v2-build" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-newer"]
    fails $ cabal "v2-build" ["--allow-newer=Foo:base"]
    fails $ cabal "v2-build" ["--allow-newer=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    cabal "v2-build" ["--allow-newer=AllowNewer:base"]
    cabal "v2-build" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"]
    cabal "v2-build" ["--allow-newer=AllowNewer:base"
                      ,"--allow-newer=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
  where
    cabal cmd args = P.cabal cmd ("--dry-run" : args)
