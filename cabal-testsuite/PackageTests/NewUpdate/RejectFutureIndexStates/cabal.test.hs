import Test.Cabal.Prelude
import Data.List (isPrefixOf)

main = cabalTest $ withProjectFile "cabal.project" $ withRemoteRepo "repo" $ do

  skip "Flaky test failing in `curl`, see #9530"

  output <- last
          . words
          . head
          . filter ("Index cache updated to index-state " `isPrefixOf`)
          . lines
          . resultOutput
        <$> recordMode DoNotRecord (cabal' "update" [])
  -- update golden output with actual timestamp
  shell "cp" ["cabal.out.in", "cabal.out"]
  shell "sed" ["-i''", "-e", "s/REPLACEME/" <> output <> "/g", "cabal.out"]
  -- This shall fail with an error message as specified in `cabal.out`
  fails $ cabal "build" ["--index-state=4000-01-01T00:00:00Z", "fake-pkg"]
  -- This shall fail by not finding the package, what indicates that it
  -- accepted an older index-state.
  fails $ cabal "build" ["--index-state=2023-01-01T00:00:00Z", "fake-pkg"]
