import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo
import           Data.Maybe

main = cabalTest $ do
  r <- cabal' "status" ["--output-format=json", "--compiler"]
  statusInfo <- withJsonOutput r
  assertBool "Must contain compiler information" (isJust $ siCompiler statusInfo)
