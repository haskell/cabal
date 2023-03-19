import Test.Cabal.Prelude
import Data.List ( isInfixOf, groupBy )
import Data.Function ( on )

main = cabalTest $ do
    workdir <- fmap testWorkDir getTestEnv
    let conf = workdir </> "cabal-config"
    cabalG ["--config-file", conf] "user-config" ["init"]
    confContents <- liftIO $ readFile conf
    let ls = lines confContents
        sections = groupBy ((==) `on` (== "")) ls
        [initLs] = filter ((== "init") . head) sections
        init = unlines initLs
    assertInitSectionContainsField init "quiet"
    assertInitSectionContainsField init "no-comments"
    assertInitSectionContainsField init "minimal"
    assertInitSectionContainsField init "simple"

assertInitSectionContainsField section field =
    assertBool ("init section of config should contain the field " ++ field)
      ((field ++ ":") `isInfixOf` section)
