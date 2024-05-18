{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path (makeRelativePathEx)

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) cyclicPreBuildRules
          }
    }

cyclicPreBuildRules :: PreBuildComponentInputs -> RulesM ()
cyclicPreBuildRules (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) = mdo
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      action = mkCommand (static Dict) (static (\ () -> error "This should not run")) ()
  r1 <- registerRule "r1" $
    staticRule action
      [ RuleDependency $ RuleOutput r2 0 ]
      ( Location autogenDir (makeRelativePathEx "G1.hs") NE.:| [] )
  r2 <- registerRule "r2" $
    staticRule action
      [ RuleDependency $ RuleOutput r1 0 ]
      ( Location autogenDir (makeRelativePathEx "G2.hs") NE.:| [] )
  r3 <- registerRule "r3" $
    staticRule action
      [ RuleDependency $ RuleOutput r3 0 ]
      ( Location autogenDir (makeRelativePathEx "G3.hs") NE.:| [] )
  return ()
