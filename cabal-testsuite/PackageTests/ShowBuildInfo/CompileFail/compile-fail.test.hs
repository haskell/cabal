{-# LANGUAGE OverloadedStrings #-}
import           Test.Cabal.Prelude
import           Test.Cabal.DecodeShowBuildInfo
import           Test.Cabal.Plan
import           Control.Monad.Trans.Reader

main = cabalTest $ do
  -- Leaf component fails to compile, should still dump
  -- build info for both components.
  fails $ runShowBuildInfo ["test:CompileFail-test"]
  withPlan $ do
    -- Lib has to be built, thus info is dumped
    assertComponent "CompileFail" mainLib
      defCompAssertion
        { modules = ["MyLib"]
        , sourceDirs = ["src"]
        }

    -- Build Info is still dumped, although compilation failed
    assertComponent "CompileFail" (test "CompileFail-test")
      defCompAssertion
        { sourceFiles = ["Main.hs"]
        , sourceDirs = ["test"]
        }

  fails $ runShowBuildInfo ["exe:CompileFail-exe"]
  withPlan $ do
    -- Internal Lib has to be built, thus info is dumped
    assertComponent "CompileFail" (lib "failing")
      defCompAssertion
        { modules = ["MyLib2"]
        , sourceDirs = ["src"]
        }
    -- However, since the internal lib failed to compile
    -- we can not have executable build information.
    Just plan <- fmap testPlan ask
    let fp = buildInfoFile plan "CompileFail" (exe "CompileFail-exe")
    shouldNotExist fp
