{-# LANGUAGE OverloadedStrings #-}
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Test.Cabal.Prelude
import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Plan
import Data.Maybe (mapMaybe)

getRevisionFor :: PackageName -> InstallItem -> Maybe Revision
getRevisionFor pkgName (AConfiguredGlobal configuredGlobal)
  | configuredGlobalPackageName configuredGlobal == pkgName =
      Just $ configuredGlobalRevision configuredGlobal
getRevisionFor _ _ = Nothing

main = do
  skipIfWindows "Mysteriously hangs in CI"
  cabalTest $ recordMode DoNotRecord $ do
    let go = do
          cabal "build" ["--dry-run", "all"]
          withPlan $ do
            Just plan <- testPlan `fmap` getTestEnv
            let [fooRev] = mapMaybe (getRevisionFor $ mkPackageName "foo") $ planInstallPlan plan
            let [barRev] = mapMaybe (getRevisionFor $ mkPackageName "bar") $ planInstallPlan plan
            assertEqual "revision of package foo" fooRev $ Revision 0
            assertEqual "revision of package bar" barRev $ Revision 1337
    withRepo "repo" go
    cabal "clean" []
    withRemoteRepo "repo" $ do
      cabal "update" []
      go
