import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo []
    assertCommonBuildInfo buildInfo
    let comps = components buildInfo
    assertEqual "Number of Components" 2 (length comps)
    assertBool "Contains main component executable"
        (any (\c -> "exe:A" == componentName c) comps)
    assertBool "Contains main component library"
        (any (\c -> "lib" == componentName c) comps)
