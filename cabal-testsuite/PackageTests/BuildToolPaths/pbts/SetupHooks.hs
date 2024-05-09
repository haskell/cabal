{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

{-# OPTIONS_GHC -Wall #-}

module SetupHooks ( setupHooks ) where

-- base
import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import qualified Data.List.NonEmpty as NE
  ( NonEmpty(..) )
import Data.Maybe
  ( catMaybes )
import Data.Traversable
  ( for )

-- Cabal-hooks
import Distribution.Simple.SetupHooks

-- Cabal
import Distribution.ModuleName
  ( ModuleName )
import Distribution.Simple.LocalBuildInfo
  ( mbWorkDirLBI )
import Distribution.Simple.Program
  ( runProgramCwd )
import Distribution.Simple.Program.Db
  ( lookupProgram )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose, findFileCwdWithExtension' )
import Distribution.Types.Component
  ( componentBuildInfo )
import Distribution.Types.LocalBuildInfo
  ( withPrograms )
import Distribution.Utils.Path
import Distribution.Utils.ShortText
  ( toShortText )

-- filepath
import System.FilePath
  ( takeExtension )

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
      noBuildHooks
        { preBuildComponentRules =
            Just $ rules (static ()) preBuildRules
        }
    }

-- | Runs the bt preprocessor on all .hs-pp files.
preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules
  PreBuildComponentInputs
    { buildingWhat   = what
    , localBuildInfo = lbi
    , targetInfo     = tgt
    }
  = do
      let verbosity = buildingWhatVerbosity what
          comp = targetComponent tgt
          compNm = componentName comp
          clbi = targetCLBI tgt
          bi = componentBuildInfo comp
          progDb = withPrograms lbi
          mbWorkDir = mbWorkDirLBI lbi

    -- 1. Look up the bt preprocessors.
      let customPp1Prog = simpleProgram "custom-pp1"
          mbCustomPp1 = lookupProgram customPp1Prog progDb
          customPp1 = case mbCustomPp1 of
            Just pp -> pp
            Nothing ->
              error $
                unlines
                  [ "pbts: could not find custom-pp1 pre-processor in the program database."
                  , "Component: " ++ show compNm ]
          customPp2Prog = simpleProgram "custom-pp2"
          mbCustomPp2 = lookupProgram customPp2Prog progDb
          customPp2 = case mbCustomPp2 of
            Just pp -> pp
            Nothing ->
              error $
                unlines
                  [ "pbts: could not find custom-pp2 pre-processor in the program database."
                  , "Component: " ++ show compNm ]

    -- 2. Create a command to run a preprocessor, passing input and output file locations.
      let
        ppCmd :: ConfiguredProgram -> Location -> Location
              -> Command ( Verbosity, Maybe (SymbolicPath CWD (Dir Pkg)), ConfiguredProgram, Location, Location ) ( IO () )
        ppCmd pp i o =
          mkCommand ( static Dict ) ( static ppModule )
            ( verbosity, mbWorkDir, pp, i, o )

    -- 3. Get all modules listed in the package description for this component.
      let mods = componentModules comp

    -- 4. Search whether any such module exists in the source tree
    --    with the "custom-pp" extension.
      let searchDirs = hsSourceDirs bi
      ppMbMods <-
        liftIO $
          for mods $ \ md -> do
            mbPath <- findFileCwdWithExtension' mbWorkDir [ "hs-pp1", "hs-pp2" ] searchDirs
                        ( moduleNameSymbolicPath md )
            case mbPath of
              Just ( base, rel ) ->
                return $
                  Just
                    ( md, Location base rel )
              Nothing ->
                return Nothing
      let ppMods = catMaybes ppMbMods
      liftIO $ putStrLn $ unlines $
        "pbts: hs-pp modules:"
        : ( map ( \ m -> "  - " ++ show m ) ppMods )
    -- TODO: declare the corresponding monitored files corresponding to the
    -- above search (it would be nice to be able to use findFileWithExtensionMonitored).

    -- 5. Declare a rule for each custom-pp module that runs the pre-processor.
      for_ ppMods $ \ ( md, inputLoc@(Location _inputBaseDir inputRelPath ) ) -> do
        let ext = takeExtension $ getSymbolicPath inputRelPath
            customPp = case ext of
              ".hs-pp1" -> customPp1
              ".hs-pp2" -> customPp2
              _ -> error $ "internal error: unhandled extension " ++ ext
            outputBaseLoc = autogenComponentModulesDir lbi clbi
            outputLoc =
              Location
                outputBaseLoc
                ( unsafeCoerceSymbolicPath $ replaceExtensionSymbolicPath inputRelPath "hs" )
        registerRule_ ( toShortText $ show md ) $
          staticRule ( ppCmd customPp inputLoc outputLoc ) [] ( outputLoc NE.:| [] )

ppModule :: ( Verbosity, Maybe (SymbolicPath CWD (Dir Pkg)), ConfiguredProgram, Location, Location ) -> IO ()
ppModule ( verbosity, mbWorkDir, customPp, inputLoc, outputLoc ) = do
  let inputPath  = location inputLoc
      outputPath = location outputLoc
  createDirectoryIfMissingVerbose verbosity True $
    interpretSymbolicPath mbWorkDir (takeDirectorySymbolicPath outputPath)
  runProgramCwd verbosity mbWorkDir customPp [ getSymbolicPath inputPath, getSymbolicPath outputPath ]

componentModules :: Component -> [ ModuleName ]
componentModules comp = libMods ++ otherModules ( componentBuildInfo comp )
  where
    libMods = case comp of
      CLib lib -> exposedModules lib
      _        -> []
  -- TODO: this doesn't take into account things like hs-boot or hsig files,
  -- but that's okay for this simple test.
