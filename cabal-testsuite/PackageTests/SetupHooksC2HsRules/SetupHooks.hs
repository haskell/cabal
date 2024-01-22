{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Compat.Binary
import Distribution.ModuleName
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils

import Data.Foldable ( for_ )
import Data.List ( isPrefixOf )
import qualified Data.List.NonEmpty as NE ( NonEmpty(..) )
import Data.String
import Data.Traversable ( for )
import GHC.Generics

import qualified Data.Map as Map
import System.FilePath

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules $ static preBuildRules
          }
    }

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = mdo
  let verbosity = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      buildDir = componentBuildDir lbi clbi

      computeC2HsDepsAction (C2HsDepsInput {..}) = do
        importLine : _srcLines <- lines <$> readFile (inDir </> toFilePath modNm <.> "myChs")
        let imports :: [ModuleName]
            imports
              | "imports:" `isPrefixOf` importLine
              = map fromString $ words $ drop 8 importLine
              | otherwise
              = error "Malformed MyChs file: first line should start with 'imports:'"
        warn verbosity $ "Computed C2Hs dependencies of " ++ modName modNm ++ ".myChs: "
                      ++ modNames imports
        return $
          ( [ RuleDependency $ RuleOutput rId 1
            | imp <- imports
            , let rId = ruleIds Map.! imp ]
          , imports )

      runC2HsAction (C2HsInput {..}) importModNms = do
        let modPath = toFilePath modNm
        warn verbosity $ "Running C2Hs on " ++ modName modNm ++ ".myChs.\n C2Hs dependencies: " ++ modNames importModNms
        _importLine : srcLines <- lines <$> readFile (inDir </> modPath <.> "myChs")

        rewriteFileEx verbosity (hsDir </> modPath <.> "hs") $
          unlines $ ("module " ++ modName modNm ++ " where\n") :
            (map ( ( "import " ++ ) . modName ) importModNms ++ srcLines)
        rewriteFileEx verbosity (chiDir </> modPath <.> "myChi") ""

      mkRule modNm =
        dynamicRule (static Dict)
          (mkCommand (static Dict) (static computeC2HsDepsAction) $ C2HsDepsInput { ruleIds = modToRuleId, ..})
          (mkCommand (static Dict) (static runC2HsAction) $ C2HsInput {hsDir = autogenDir, chiDir = buildDir, ..})
          [ FileDependency (inDir, modPath <.> "myChs") ]
          ( ( autogenDir, modPath <.> "hs" ) NE.:| [ ( buildDir, modPath <.> "myChi" ) ] )
        where
          modPath = toFilePath modNm
          inDir = "."

  -- NB: in practice, we would get the module names by looking at the .cabal
  -- file and performing a search for `.chs` files on disk, but for this test
  -- we bake this in for simplicity.
  let mods = Map.fromList [ ((ix, fromString modNm), ())
                          | (ix, modNm) <- [ (0, "C"), (1, "A1"), (2, "B"), (3, "A2") ] ]
    -- NB: the extra indices are to ensure the traversal happens in a particular order,
    -- which ensures we correctly re-order rules to execute them in dependency order.
  modToRuleId <- fmap (Map.mapKeys snd) $ flip Map.traverseWithKey mods $ \ (i, modNm) () ->
    registerRule ("C2Hs " <> fromString (show i ++ " " ++ modName modNm)) $ mkRule modNm
  return ()

-- | Input to C2Hs dependency computation
data C2HsDepsInput
  = C2HsDepsInput
  { verbosity :: Verbosity
  , inDir :: FilePath
  , modNm :: ModuleName
  , ruleIds :: Map.Map ModuleName RuleId
  }
  deriving stock ( Show, Generic )
  deriving anyclass Binary

-- | Input to C2Hs command
data C2HsInput
  = C2HsInput
  { verbosity :: Verbosity
  , modNm :: ModuleName
  , inDir :: FilePath
  , hsDir :: FilePath
  , chiDir :: FilePath
  }
  deriving stock ( Show, Generic )
  deriving anyclass Binary

modName :: ModuleName -> String
modName = intercalate "." . components

modNames :: [ModuleName] -> String
modNames mns = "[" ++ intercalate ", " (map modName mns) ++ "]"
