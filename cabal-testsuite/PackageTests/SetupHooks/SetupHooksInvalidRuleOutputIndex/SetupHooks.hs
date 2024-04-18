{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils ( rewriteFileEx )

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )
import System.FilePath

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) invalidRuleOutputIndexRules
          }
    }

invalidRuleOutputIndexRules :: PreBuildComponentInputs -> RulesM ()
invalidRuleOutputIndexRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = do
  let clbi = targetCLBI tgt
      i = interpretSymbolicPathLBI lbi
      autogenDir = i (autogenComponentModulesDir lbi clbi)
      verbosity = buildingWhatVerbosity what
      action = mkCommand (static Dict) $ static (\ ((dir, modNm), verb) -> do
        let loc = dir </> modNm <.> "hs"
        rewriteFileEx verb loc $
          "module " ++ modNm ++ " where {}"
        )

  r1 <- registerRule "r1" $
          staticRule
            (action ((autogenDir, "A"), verbosity))
            [] ( ( autogenDir, "A.hs" ) NE.:| [] )
  registerRule_ "r2" $
    staticRule (action ((autogenDir, "B"), verbosity))
      [ RuleDependency $ RuleOutput r1 7 ]
      ( ( autogenDir, "B.hs" ) NE.:| [] )
