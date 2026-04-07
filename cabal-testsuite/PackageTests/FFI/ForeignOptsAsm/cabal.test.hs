import Test.Cabal.Prelude
import Distribution.System (Arch (X86_64, AArch64), buildArch)

main = do
  skipUnlessIO "needs x86_64 or aarch64" (buildArch `elem` [X86_64, AArch64])
  cabalTest $ do
    cabal "v2-build" ["foreign-opts-asm-exe"]
    withPlan $ runPlanExe "foreign-opts-asm" "foreign-opts-asm-exe" []
