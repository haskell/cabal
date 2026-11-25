import Test.Cabal.Prelude

import Data.List (isPrefixOf, tails)

-- Each component of a package is a unit of its own in the plan, but they all
-- point the package's datadir variable to the same place, so the overrides
-- that the setup of each component gets list it once.
main = cabalTest $ recordMode DoNotRecord $ do
  r <- cabal' "build" ["all"]
  let count needle = length (filter (needle `isPrefixOf`) (tails (resultOutput r)))
      lists = count "Including the following environment variable overrides:"
  assertBool "the overrides are logged" (lists > 0)
  assertEqual "datadir entries, one per list" lists (count "- DataDirEnvOnce_datadir=")
