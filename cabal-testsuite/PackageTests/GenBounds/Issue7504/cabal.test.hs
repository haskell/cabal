import System.Directory (setCurrentDirectory)
import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    r <- cabal' "gen-bounds" ["all"]
    assertOutputContains "For component package-a:lib:package-a:" r
    assertOutputContains "For component package-b:lib:package-b:" r
    assertOutputContains "For component package-b:exe:package-b:" r
    assertOutputContains "text >=" r
    assertOutputContains "package-a >= 0.1.0 && < 0.2" r

