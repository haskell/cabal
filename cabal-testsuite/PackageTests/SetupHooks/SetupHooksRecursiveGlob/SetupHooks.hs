{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE RecordWildCards #-}

module SetupHooks where

import Control.Monad.IO.Class (liftIO)
import Distribution.CabalSpecVersion
import Distribution.Simple.SetupHooks
import Distribution.Simple.Glob
import Distribution.Utils.Path
import Distribution.Simple.LocalBuildInfo (componentBuildInfo, mbWorkDirLBI)

import Data.Traversable ( for )

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) preBuildRules
          }
    }

-- Register a pre-build rule that uses a recursive glob.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules PreBuildComponentInputs {..} = do
    let cabalVersion = CabalSpecV3_16
        verbosity = buildingWhatVerbosity buildingWhat
        comp = targetComponent targetInfo
        bi = componentBuildInfo comp
        mbWorkDir = mbWorkDirLBI localBuildInfo
    let globFilename = "**/*.ppExt" 
    let glob = case parseFileGlob cabalVersion globFilename of
                Left err ->
                  error $ explainGlobSyntaxError globFilename err
                Right glob ->
                  glob
    myPpFiles <- liftIO $ for ( hsSourceDirs bi ) $ \ srcDir -> do
      let root = interpretSymbolicPath mbWorkDir srcDir
      matches <- runDirFileGlob verbosity Nothing root glob
      return
        [ Location srcDir ( makeRelativePathEx match )
        | match <- globMatches matches
        ]
    -- Monitor existence of file glob to handle new input files getting added.
    addRuleMonitors [ monitorFileGlobExistence $ RootedGlob FilePathRelative glob ]
