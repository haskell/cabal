{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) unusedPreBuildRules
          }
    }

unusedPreBuildRules :: PreBuildComponentInputs -> RulesM ()
unusedPreBuildRules (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) = do
  let clbi = targetCLBI tgt
      i = interpretSymbolicPathLBI lbi
      autogenDir = i (autogenComponentModulesDir lbi clbi)
      action = mkCommand (static Dict) (static (\ _ -> error "This should not run")) ()
  registerRule_ "r1" $
    staticRule action []
      ( ( autogenDir, "X.hs" ) NE.:| [ ( autogenDir, "Y.hs" ) ] )
  registerRule_ "r2" $
    staticRule action []
      ( ( autogenDir, "Z.what" ) NE.:| [] )
