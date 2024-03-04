{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.SetupHooks

import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules $ static dupRuleIdRules
          }
    }

dupRuleIdRules :: PreBuildComponentInputs -> RulesM ()
dupRuleIdRules _ = do
  let cmd = mkCommand (static Dict) (static (\ _ -> error "This should not run")) ()
  registerRule_ "myRule" $ staticRule cmd [] ( ( "src", "A.hs" ) NE.:| [] )
  registerRule_ "myRule" $ staticRule cmd [] ( ( "src", "B.hs" ) NE.:| [] )
