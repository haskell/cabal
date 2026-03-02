{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils ( rewriteFileEx, warn )
import Distribution.Utils.Path
import Distribution.Verbosity

import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )
import Data.Traversable ( for )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) preBuildRules
          }
    }

-- Register three rules:
--
-- r1: B --> C
-- r2: A --> B
-- r3: C --> D
--
-- and check that we run them in dependency order, i.e. r2, r1, r3.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = mdo
  let verbosityFlags = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi

      mkAction =
        mkCommand (static Dict) $ static (\ (dir, verbFlags, (inMod, outMod)) -> do
          let verbosity = mkVerbosity defaultVerbosityHandles verbFlags
          warn verbosity $ "Running rule: " ++ inMod ++ " --> " ++ outMod
          let loc = getSymbolicPath dir </> outMod <.> "hs"
          rewriteFileEx verbosity loc $
            "module " ++ outMod ++ " where { import " ++ inMod ++ " }"
        )

      actionArg inMod outMod =
        (autogenDir, verbosityFlags, (inMod, outMod))

      mkRule action input outMod =
        staticRule action
          [ input ]
          ( Location autogenDir (makeRelativePathEx outMod <.> "hs") NE.:| [] )

  -- B --> C
  -- A --> B
  -- C --> D
  r1 <- registerRule "r1" $ mkRule (mkAction (actionArg "B" "C")) (RuleDependency $ RuleOutput r2 0) "C"
  r2 <- registerRule "r2" $ mkRule (mkAction (actionArg "A" "B")) (FileDependency $ Location sameDirectory (makeRelativePathEx "A.hs")) "B"
  r3 <- registerRule "r3" $ mkRule (mkAction (actionArg "C" "D")) (RuleDependency $ RuleOutput r1 0) "D"
  return ()
