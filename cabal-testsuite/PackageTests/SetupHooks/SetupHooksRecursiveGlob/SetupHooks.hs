{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Distribution.CabalSpecVersion
import Distribution.Compat.Binary
import Distribution.Simple.Glob
import Distribution.Simple.LocalBuildInfo (componentBuildInfo, mbWorkDirLBI)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Utils.ShortText (toShortText)
import Distribution.Verbosity

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Traversable (for)
import GHC.Generics
import System.FilePath

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) preBuildRules
          }
    }

data PPArgs
  = PPArgs
  { verbosityFlags :: VerbosityFlags
  , srcPath :: FilePath
  , dstPath :: FilePath
  }
  deriving stock (Show, Generic)
  deriving anyclass (Binary)

-- Register a pre-build rule that uses a recursive glob.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules PreBuildComponentInputs{..} = do
  let cabalVersion = CabalSpecV3_16
      verbosityFlags = buildingWhatVerbosity buildingWhat
      comp = targetComponent targetInfo
      bi = componentBuildInfo comp
      mbWorkDir = mbWorkDirLBI localBuildInfo
      clbi = targetCLBI targetInfo
      autogenDir = autogenComponentModulesDir localBuildInfo clbi
  -- buildDir = componentBuildDir localBuildInfo clbi

  let globFilename = "**/*.ppExt"
  let glob = case parseFileGlob cabalVersion globFilename of
        Left err ->
          error $ explainGlobSyntaxError globFilename err
        Right glob ->
          glob
  myPpFiles <- fmap concat $ liftIO $ for (hsSourceDirs bi) $ \srcDir -> do
    let root = interpretSymbolicPath mbWorkDir srcDir
    let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
    matches <- runDirFileGlob verbosity Nothing root glob
    return
      [ Location srcDir (makeRelativePathEx match)
      | match <- globMatches matches
      ]
  -- Monitor existence of file glob to handle new input files getting added.
  addRuleMonitors [monitorFileGlobExistence $ RootedGlob FilePathRelative glob]

  let preprocessFile PPArgs{..} = do
        let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
        warn verbosity $ "Preprocessing: " ++ srcPath ++ " -> " ++ dstPath
        (modLine : inputLines) <- lines <$> readFile srcPath
        let hsSrc =
              unlines
                [ modLine
                , ""
                , "str :: String"
                , "str = " ++ show (unlines inputLines)
                ]
        createDirectoryIfMissingVerbose verbosity True (takeDirectory dstPath)
        rewriteFileEx verbosity dstPath hsSrc

  -- Register preprocessor rules
  forM_ myPpFiles $ \ppFile@(Location _ relPath) -> do
    let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
    let ppFile' =
          Location
            autogenDir
            (replaceExtensionSymbolicPath (unsafeCoerceSymbolicPath relPath) "hs")

    let action =
          mkCommand
            (static Dict)
            (static preprocessFile)
            PPArgs
              { srcPath = interpretSymbolicPath mbWorkDir (location ppFile)
              , dstPath = interpretSymbolicPath mbWorkDir (location ppFile')
              , ..
              }

    registerRule ("myPP " <> toShortText (getSymbolicPath relPath)) $
      staticRule
        action
        [FileDependency ppFile]
        (ppFile' :| [])
