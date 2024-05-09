{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
          { preBuildComponentRules = Just $ rules (static ()) unusedPreBuildRules
          }
    }

unusedPreBuildRules :: PreBuildComponentInputs -> RulesM ()
unusedPreBuildRules (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) = do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      action = mkCommand (static Dict) (static (\ _ -> error "This should not run")) ()
  registerRule_ "r1" $
    staticRule action []
      ( Location autogenDir (makeRelativePathEx "X.hs") NE.:| [ Location autogenDir (makeRelativePathEx "Y.hs") ] )
  registerRule_ "r2" $
    staticRule action []
      ( Location autogenDir (makeRelativePathEx "Z.hs") NE.:| [] )
