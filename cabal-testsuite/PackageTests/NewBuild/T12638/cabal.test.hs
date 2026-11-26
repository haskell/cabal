import System.Directory (createDirectoryIfMissing)
import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    env <- getTestEnv
    liftIO $ createDirectoryIfMissing True (testCurrentDir env </> "repo")

    let args = ["--dry-run", "--offline"]
    latest <- cabal' "v2-build" args
    assertOutputDoesNotContain "index-state ignored for file+noindex" latest

    historical <- cabal' "v2-build" (args ++ ["--index-state=2026-06-01T00:00:00Z"])
    assertOutputContains "index-state ignored for file+noindex repository 'local-a'" historical
    assertOutputContains "index-state ignored for file+noindex repository 'local-b'" historical
