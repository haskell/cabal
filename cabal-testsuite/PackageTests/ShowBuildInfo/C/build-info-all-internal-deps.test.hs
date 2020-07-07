import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo

main = cabalTest $ do
    buildInfo <- runShowBuildInfo ["-v0"]
    let comps = components buildInfo
    assertEqual "Components, exactly three" 2  (length comps)
    assertEqual "Test components, exactly one" 1 $
        length $ filter (\c -> "test" == componentType c) comps
