import Test.Cabal.Prelude
import Distribution.System (Arch (..), buildArch, OS (..), buildOS)

main = do
  skipUnlessIO "needs x86_64 or aarch64"
    ( (buildArch == X86_64 && buildOS == Windows)
    || (buildArch == X86_64 && buildOS == Linux)
    || (buildArch == AArch64 && buildOS == OSX)
    )
  cabalTest $ recordMode DoNotRecord $ do
    cabal "v2-build" ["foreign-opts-asm-exe"]
    withPlan $ runPlanExe "foreign-opts-asm" "foreign-opts-asm-exe" []
