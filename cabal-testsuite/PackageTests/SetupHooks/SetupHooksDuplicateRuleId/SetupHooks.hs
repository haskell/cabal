{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.SetupHooks
import Distribution.Utils.Path (sameDirectory, makeRelativePathEx)

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) dupRuleIdRules
          }
    }

dupRuleIdRules :: PreBuildComponentInputs -> RulesM ()
dupRuleIdRules _ = do
  let cmd = mkCommand (static Dict) (static (\ _ -> error "This should not run")) ()
  registerRule_ "myRule" $ staticRule cmd [] ( Location sameDirectory (makeRelativePathEx "A.hs") NE.:| [] )
  registerRule_ "myRule" $ staticRule cmd [] ( Location sameDirectory (makeRelativePathEx "B.hs") NE.:| [] )
