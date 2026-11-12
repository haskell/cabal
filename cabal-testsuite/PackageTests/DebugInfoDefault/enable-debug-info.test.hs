{-# LANGUAGE OverloadedStrings #-}

import Test.Cabal.DecodeShowBuildInfo
import Test.Cabal.Prelude

-- Regression test for the default debug-info level being NoDebugInfo.
--
-- The GHC command line must only carry a debug-info flag (-g1, -g2 or
-- -g3) when a level is explicitly requested. The bare -g and
-- --enable-debug-info forms fall back to the default level, which is
-- NoDebugInfo.
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

    -- Bare --enable-debug-info falls back to NoDebugInfo.
    runShowBuildInfo ["--enable-debug-info", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = notElem "-g2"
              }
        )

    -- Bare -g falls back to NoDebugInfo, too.
    runShowBuildInfo ["-g", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = notElem "-g2"
              }
        )

    -- Explicit --disable-debug-info emits no -g2 either.
    runShowBuildInfo ["--disable-debug-info", "lib:debug-info-default"]
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

    -- -g1 selects MinimalDebugInfo (-g1) and -g3 selects
    -- MaximalDebugInfo (-g3).
    runShowBuildInfo ["-g1", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = elem "-g1"
              }
        )
    runShowBuildInfo ["-g3", "lib:debug-info-default"]
      >> withPlan
        ( assertComponent "debug-info-default" mainLib $
            defCompAssertion
              { modules = ["MyLib"]
              , sourceDirs = ["src"]
              , compilerArgsPred = elem "-g3"
              }
        )
