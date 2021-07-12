import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["-v0"] -- hide verbose output so we can parse
    let comps = components buildInfo
    assertEqual "Components, exactly three" 3  (length comps)
    assertEqual "Test components, exactly one" 1 $
        length $ filter (\c -> "test" == componentType c) comps
