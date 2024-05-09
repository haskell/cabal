{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (rewriteFileEx)
import Distribution.Utils.Path

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

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
      autogenDir = autogenComponentModulesDir lbi clbi
      verbosity = buildingWhatVerbosity what
      action = mkCommand (static Dict) $ static (\ ((dir, modNm), verb) -> do
        let loc = getSymbolicPath dir </> modNm <.> "hs"
        rewriteFileEx verb loc $
          "module " ++ modNm ++ " where {}"
        )

  r1 <- registerRule "r1" $
          staticRule
            (action ((autogenDir, "A"), verbosity))
            [] ( Location autogenDir (makeRelativePathEx "A.hs") NE.:| [] )
  registerRule_ "r2" $
    staticRule (action ((autogenDir, "B"), verbosity))
      [ RuleDependency $ RuleOutput r1 7 ]
      ( Location autogenDir (makeRelativePathEx "B.hs") NE.:| [] )
