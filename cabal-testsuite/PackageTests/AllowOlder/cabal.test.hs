import Test.Cabal.Prelude hiding (cabal)
import qualified Test.Cabal.Prelude as P
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ do
    fails $ cabal "v2-build" []
    cabal "v2-build" ["--allow-older"]
    fails $ cabal "v2-build" ["--allow-older=baz,quux"]
    cabal "v2-build" ["--allow-older=base", "--allow-older=baz,quux"]
    cabal "v2-build" ["--allow-older=bar", "--allow-older=base,baz"
                      ,"--allow-older=quux"]
    fails $ cabal "v2-build" ["--enable-tests"]
    cabal "v2-build" ["--enable-tests", "--allow-older"]
    fails $ cabal "v2-build" ["--enable-benchmarks"]
    cabal "v2-build" ["--enable-benchmarks", "--allow-older"]
    fails $ cabal "v2-build" ["--enable-benchmarks", "--enable-tests"]
    cabal "v2-build" ["--enable-benchmarks", "--enable-tests"
                      ,"--allow-older"]
    fails $ cabal "v2-build" ["--allow-older=Foo:base"]
    fails $ cabal "v2-build" ["--allow-older=Foo:base"
                                   ,"--enable-tests", "--enable-benchmarks"]
    cabal "v2-build" ["--allow-older=AllowOlder:base"]
    cabal "v2-build" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"]
    cabal "v2-build" ["--allow-older=AllowOlder:base"
                      ,"--allow-older=Foo:base"
                      ,"--enable-tests", "--enable-benchmarks"]
  where
    cabal cmd args = P.cabal cmd ("--dry-run" : args)
