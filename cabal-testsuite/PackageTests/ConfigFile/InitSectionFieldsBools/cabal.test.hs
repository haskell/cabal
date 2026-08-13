import Test.Cabal.Prelude
import Data.List (isInfixOf, groupBy)
import Data.Function (on)

main = cabalTest $ do
  workdir <- fmap testWorkDir getTestEnv
  let conf = workdir </> "cabal-config"

  cabalG ["--config-file", conf] "user-config" ["init"]
  confContents <- liftIO $ readFile conf

  let ls = lines confContents
      sections = groupBy ((==) `on` (== "")) ls
      [initLs] = filter ((== "-- full-version: False") . head) sections
      init = unlines initLs

  assertBool "init section of config should contain debug-info: 0" ("debug-info: 0" `isInfixOf` init)
  assertBool "init section of config should contain optimization: 1" ("optimization: 1" `isInfixOf` init)
