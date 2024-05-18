{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path (sameDirectory, makeRelativePathEx)

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) missingDepRules
          }
    }

missingDepRules :: PreBuildComponentInputs -> RulesM ()
missingDepRules (PreBuildComponentInputs { localBuildInfo = lbi, targetInfo = tgt }) = do
  let clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      action = mkCommand (static Dict) (static (\ _ -> error "This should not run")) ()
  registerRule_ "r" $
    staticRule action
      [ FileDependency $ Location sameDirectory (makeRelativePathEx "Missing.hs") ]
      ( Location autogenDir (makeRelativePathEx "G.hs") NE.:| [] )
