{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

-- Cabal
import Distribution.Compat.Binary
import Distribution.Parsec
  ( simpleParsec )
import Distribution.Simple.LocalBuildInfo
  ( interpretSymbolicPathLBI )
import Distribution.Simple.Utils
  ( warn, rewriteFileEx )
import Distribution.Utils.Path
import Distribution.Verbosity

-- Cabal-hooks
import Distribution.Simple.SetupHooks

-- base
import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import Data.List
  ( isSuffixOf )
import qualified Data.List.NonEmpty as NE
  ( NonEmpty(..) )
import Data.String
  ( fromString )
import GHC.Generics

-- directory
import System.Directory
  ( listDirectory )

-- filepath
import System.FilePath
  ( dropExtension )

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) preBuildRules
          }
    }

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = do
  let verbosityFlags = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      srcDir = sameDirectory

  -- Monitor .myPP files in the package directory.
  let myPPGlob =
        case simpleParsec "*.myPP" of
          Just g  -> g
          Nothing -> error "SetupHooksRecompRules: failed to parse *.myPP glob"
  addRuleMonitors [ monitorFileGlobExistence myPPGlob ]

  -- Scan the package directory for .myPP files and register one
  -- preprocessing rule per file.
  allFiles <- liftIO $ listDirectory (interpretSymbolicPathLBI lbi srcDir)
  for_ (filter (".myPP" `isSuffixOf`) allFiles) $ \fileName -> do
    let baseName = dropExtension fileName
        -- For A and B, bake in a constant verbosity so that their rules are
        -- unaffected by the --verbose flag. C uses the actual verbosity, so
        -- its rule changes when the verbosity changes.
        ruleVerbosityFlags
          | baseName `elem` ["A", "B"] = normal
          | otherwise                  = verbosityFlags
    registerRule_ (fromString $ "myPP " ++ baseName) $
      staticRule
        (mkCommand (static Dict) (static runMyPP) $
          MyPPInput
            { ppVerbosityFlags = ruleVerbosityFlags
            , ppSrcDir         = srcDir
            , ppAutogenDir     = autogenDir
            , ppBaseName       = baseName
            })
        [ FileDependency $ Location srcDir (makeRelativePathEx fileName) ]
        ( Location autogenDir (makeRelativePathEx baseName <.> "hs") NE.:| [] )

-- | Preprocess a single .myPP file into a .hs module.
runMyPP :: MyPPInput -> IO ()
runMyPP (MyPPInput {..}) = do
  let verbosity = mkVerbosity defaultVerbosityHandles ppVerbosityFlags
  warn verbosity $ "Running myPP preprocessor for " ++ ppBaseName
  content <- readFile (getSymbolicPath ppSrcDir </> ppBaseName <.> "myPP")
  rewriteFileEx verbosity (getSymbolicPath ppAutogenDir </> ppBaseName <.> "hs") $
    "module " ++ ppBaseName ++ " where\n" ++ content

data MyPPInput
  = MyPPInput
  { ppVerbosityFlags :: VerbosityFlags
  , ppSrcDir         :: SymbolicPath Pkg (Dir Source)
  , ppAutogenDir     :: SymbolicPath Pkg (Dir Source)
  , ppBaseName       :: String
  }
  deriving stock    ( Show, Generic )
  deriving anyclass Binary
