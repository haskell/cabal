{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

-- Regression test for the default debug-info level being NoDebugInfo.
--
-- The GHC command line must only carry a debug-info flag (-g2) when a
-- level is explicitly requested, not by default nor via a bare
-- --enable-debug-info.
main = cabalTest $ do
  recordMode DoNotRecord $ do
    -- Default: NoDebugInfo, no -g2.
    runShowBuildInfo ["lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = notElem "-g2"
              }
        )

    -- --enable-debug-info without a level maps to NoDebugInfo.
    runShowBuildInfo ["--enable-debug-info", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = notElem "-g2"
              }
        )

    -- --enable-debug-info=2 selects NormalDebugInfo, which emits -g2.
    runShowBuildInfo ["--enable-debug-info=2", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = elem "-g2"
              }
        )
