module UnitTests.Distribution.Client.Init
  ( tests
  ) where

import Test.Tasty

import qualified UnitTests.Distribution.Client.Init.FileCreators as FileCreators
import qualified UnitTests.Distribution.Client.Init.Golden as Golden
import qualified UnitTests.Distribution.Client.Init.Interactive as Interactive
import qualified UnitTests.Distribution.Client.Init.NonInteractive as NonInteractive
import qualified UnitTests.Distribution.Client.Init.Simple as Simple

import UnitTests.Distribution.Client.Init.Utils

import Distribution.Client.Config
import Distribution.Client.IndexUtils
import Distribution.Client.Init.Types
import Distribution.Client.Sandbox
import Distribution.Client.Setup
import Distribution.Verbosity

tests :: IO [TestTree]
tests = do
  confFlags <- loadConfigOrSandboxConfig v defaultGlobalFlags

  let confFlags' = savedConfigureFlags confFlags `mappend` compFlags
      initFlags' = savedInitFlags confFlags `mappend` emptyFlags
      globalFlags' = savedGlobalFlags confFlags `mappend` defaultGlobalFlags

  (comp, _, progdb) <- configCompilerAux' confFlags'

  withRepoContext v globalFlags' $ \repoCtx -> do
    let pkgDb = configPackageDB' confFlags'

    pkgIx <- getInstalledPackages v comp pkgDb progdb
    srcDb <- getSourcePackages v repoCtx

    return
      [ Interactive.tests v initFlags' pkgIx srcDb
      , NonInteractive.tests v initFlags' comp pkgIx srcDb
      , Golden.tests v initFlags' pkgIx srcDb
      , Simple.tests v initFlags' pkgIx srcDb
      , FileCreators.tests v initFlags' comp pkgIx srcDb
      ]
  where
    v :: Verbosity
    v = normal

    compFlags :: ConfigFlags
    compFlags = mempty{configHcPath = initHcPath emptyFlags}
