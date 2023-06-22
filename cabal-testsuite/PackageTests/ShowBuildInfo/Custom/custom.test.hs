{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Reader
import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

main = setupTest $ do
  -- No cabal test because per-component is broken with it
  skipUnlessGhcVersion ">= 8.1"
  withPackageDb $ do
    setup_build ["--enable-build-info"]
    env <- ask
    let buildInfoFp = testDistDir env </> "build-info.json"
    buildInfo <- decodeBuildInfoFile buildInfoFp
    assertCommonBuildInfo buildInfo
    let [libBI, exeBI] = components buildInfo

    assertComponentPure
      libBI
      defCompAssertion
        { modules = ["MyLib"]
        , compType = "lib"
        , sourceDirs = ["src"]
        }

    assertComponentPure
      exeBI
      defCompAssertion
        { sourceFiles = ["Main.hs"]
        , compType = "exe"
        , sourceDirs = ["app"]
        }
