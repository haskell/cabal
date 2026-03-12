{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Distribution.Compat.Binary
import Distribution.ModuleName
import Distribution.Simple.LocalBuildInfo
  ( interpretSymbolicPathLBI )
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils
import Distribution.Types.LocalBuildInfo
  ( buildDirPBD )
import Distribution.Types.UnqualComponentName
  ( unUnqualComponentName )
import Distribution.Utils.Path
import Distribution.Verbosity

import Control.Monad ( void )
import Data.Foldable ( for_ )
import Data.List ( isPrefixOf )
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.Traversable ( for )
import GHC.Generics

import qualified Data.Map as Map

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
         { preConfComponentHook = Just pcc }
    , buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) preBuildRules
          }
    }

pcc :: PreConfComponentHook
pcc (PreConfComponentInputs _lbc pbd _comp) =
  return $
    PreConfComponentOutputs $ ComponentDiff $ CExe $
      emptyExecutable
        { buildInfo =
            -- Need to add the .c files, so that they get included in the final
            -- linking invocation.
            --
            -- For the .h file:
            --
            --  - We don't need it at configure time, so we generate it in a pre-build rule.
            --    We can't add it to 'includes'/'autogenIncludes', as Cabal would go looking for it
            --    at configure time (before we run pre-build rules).
            --  - If we needed it at configure time, we would need to generate it
            --    in this per-component pre-configure hook and then add it to 'includes'/'autogenIncludes'.
            --    That would work, but would mean we wouldn't benefit from
            --    recompilation checking.
            emptyBuildInfo
              { cSources = [ autogenDir </> unsafeMakeSymbolicPath "Gen.c"
                           , autogenDir </> unsafeMakeSymbolicPath "Gen2.c"]
              }
        }
  where
    autogenDir = buildDirPBD pbd </> (unsafeMakeSymbolicPath "NonHs/autogen")

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules (PreBuildComponentInputs { buildingWhat = what, localBuildInfo = lbi, targetInfo = tgt }) = mdo
  let verbosityFlags = buildingWhatVerbosity what
      clbi = targetCLBI tgt
      autogenDir = autogenComponentModulesDir lbi clbi
      buildDir = componentBuildDir lbi clbi

      runPpAction1 (PpInput {..}) = do
        let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
        warn verbosity "Running MyPp1"
        rewriteFileEx verbosity (getSymbolicPath genDir </> "Gen.h") $ unlines
          [ "#include \"Bot.h\""
          , "int gen_quux(int);"
          , "int gen_nozzle(int);"
          , "int norbert(int);"
          ]
        rewriteFileEx verbosity (getSymbolicPath genDir </> "Gen.c") $ unlines
          [ "#include \"A_stub.h\""
          , "#include \"B_stub.h\""
          , "int gen_quux(int x) { return (foo(x) + bar(x)); };"
          , "int gen_nozzle(int x) { return (x + wobble(x)); };"
          , "int norbert(int x) { return (x+x); };"
          ]

      runPpAction2 (PpInput {..}) = do
        let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
        warn verbosity "Running MyPp2"
        rewriteFileEx verbosity (getSymbolicPath genDir </> "B.hs") $ unlines
          [ "{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}"
          , "module B where"
          , "import Foreign.C.Types (CInt(..))"
          , "foo :: CInt -> CInt"
          , "foo x = 2 * x + 1"
          , ""
          , "foreign export ccall foo :: CInt -> CInt"
          , "foreign import capi \"Gen.h norbert\" norbert :: CInt -> CInt"
          , ""
          , "foreign import ccall \"is_needed\" isNeeded :: Int -> Int"
          ]

      -- Check that this rule is demanded via the cSources demand.
      -- No other rule demands it.
      runPpAction3 (PpInput {..}) = do
        let verbosity = mkVerbosity defaultVerbosityHandles verbosityFlags
        warn verbosity "Running MyPp3"
        rewriteFileEx verbosity (getSymbolicPath genDir </> "Gen2.c") $ unlines
          [ "int is_needed(int x) { return (x+ 999000); };"
          ]

      mkRule1 =
        staticRule
          (mkCommand (static Dict) (static runPpAction1) $ PpInput {genDir = autogenDir, ..})
          [ ]
          ( Location autogenDir (unsafeMakeSymbolicPath "Gen.h") NE.:|
          [ Location autogenDir (unsafeMakeSymbolicPath "Gen.c")
          ] )

      mkRule2 dep =
        staticRule
          (mkCommand (static Dict) (static runPpAction2) $ PpInput {genDir = autogenDir, ..})
          [ RuleDependency (RuleOutput dep 0) ]
          ( Location autogenDir (unsafeMakeSymbolicPath "B.hs") NE.:| [] )

      mkRule3 =
        staticRule
          (mkCommand (static Dict) (static runPpAction3) $ PpInput {genDir = autogenDir, ..})
          [ ]
          ( NE.singleton $ Location autogenDir (unsafeMakeSymbolicPath "Gen2.c") )

  r1 <- registerRule "MyPP1" mkRule1
  void $ registerRule "MyPP2" (mkRule2 r1)
  void $ registerRule "MyPP3" mkRule3

-- | Input to preprocessor command
data PpInput
  = PpInput
  { verbosityFlags :: VerbosityFlags
  , genDir         :: SymbolicPath Pkg (Dir Source)
  }
  deriving stock ( Show, Generic )
  deriving anyclass Binary

modName :: ModuleName -> String
modName = intercalate "." . components

modNames :: [ModuleName] -> String
modNames mns = "[" ++ intercalate ", " (map modName mns) ++ "]"
