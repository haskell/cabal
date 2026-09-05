{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Distribution.Backpack
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ModuleRenaming
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity

import Data.List (isPrefixOf)

import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { confHook = \args flags -> do
        lbi <- confHook simpleUserHooks args flags
        generateScriptEnvModule lbi (fromFlagOrDefault minBound (configVerbosity flags))
        pure lbi
    }

generateScriptEnvModule :: LocalBuildInfo -> Verbosity -> IO ()
generateScriptEnvModule lbi verbosity = do
    lbiPackageDbStack <- mapM canonicalizePackageDB (withPackageDB lbi)

    createDirectoryIfMissing True moduledir
    rewriteFileEx verbosity (moduledir </> "ScriptEnv0.hs") $ unlines
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE FlexibleInstances #-}"
      , "{-# OPTIONS_GHC -Wno-orphans   #-}"
      , "module Test.Cabal.ScriptEnv0 where"
      , ""
      , "import Distribution.Simple"
      , "import Distribution.System (Platform(..), Arch(..), OS(..))"
      , "import Distribution.Types.ModuleRenaming"
      , "import Distribution.Simple.Program.Db"
      , "import Distribution.Backpack (OpenUnitId)"
      , "import Data.Map (fromList)"
      , "import Data.String (IsString(..))"
      , "import Distribution.Utils.Path"
      , ""
      , "lbiPackageDbStack :: PackageDBStackCWD"
      , "lbiPackageDbStack = " ++ show lbiPackageDbStack
      , ""
      , "lbiPlatform :: Platform"
      , "lbiPlatform = " ++ show (hostPlatform lbi)
      , ""
      , "lbiCompiler :: Compiler"
      -- We added a new field to compiler so we need to be careful
      -- to make sure that it is always defined,
      -- even if the test suite is being built with an older Cabal
#if MIN_VERSION_Cabal(3,15,0)
      , "lbiCompiler = " ++ show (compiler lbi)
#else
      , "lbiCompiler = " ++ init (show (compiler lbi)) ++ ", compilerWiredInUnitIds = Nothing}"
#endif
      , ""
      , "lbiPackages :: [(OpenUnitId, ModuleRenaming)]"
      , "lbiPackages = read " ++ show (show (cabalTestsPackages lbi))
      , ""
      , "lbiProgramDb :: ProgramDb"
      -- The `Show`/`Read` round trip below crosses Cabal versions: this
      -- setup script is compiled against the released Cabal pinned in the
      -- `custom-setup` stanza, while the generated module is read by the
      -- test suite compiled against the in-tree Cabal, whose types may
      -- have gained fields. Splice the fields added to `ConfiguredProgram`
      -- since the pinned release, so that the newer `Read` accepts the
      -- generated value (in the spirit of the `lbiCompiler` workaround
      -- above).
      , "lbiProgramDb = read " ++ show (addMissingConfiguredProgramFields (show (withPrograms lbi)))
      , ""
      , "lbiWithSharedLib :: Bool"
      , "lbiWithSharedLib = " ++ show (withSharedLib lbi)
      , ""
      , "instance IsString (SymbolicPath from to) where"
      , "  fromString = makeSymbolicPath"
      ]
  where
    moduledir = libAutogenDir </> "Test" </> "Cabal"
    -- fixme: use component-specific folder
    libAutogenDir = autogenPackageModulesDir lbi

-- | `programDriverArgs` was added to `ConfiguredProgram` after the Cabal
-- release this setup script is compiled against; its derived `Show`
-- therefore omits the field that the test suite's newer `Read` expects.
-- Splice it into every record of the generated `show` output.
--
-- The spliced label is assumed not to occur inside any of the shown string
-- values, which holds for the program arguments occurring in practice.
addMissingConfiguredProgramFields :: String -> String
addMissingConfiguredProgramFields = go
  where
    needle = ", programOverrideEnv = "
    replacement = ", programDriverArgs = [], programOverrideEnv = "
    go [] = []
    go s@(c : rest)
      | needle `isPrefixOf` s = replacement ++ go (drop (length needle) s)
      | otherwise = c : go rest

-- | Convert package database into absolute path, so that
-- if we change working directories in a subprocess we get the correct database.
canonicalizePackageDB :: PackageDB -> IO PackageDB
canonicalizePackageDB (SpecificPackageDB path)
    = SpecificPackageDB `fmap` canonicalizePath path
canonicalizePackageDB x = return x

-- | Compute the set of @-package-id@ flags which would be passed when
-- building the public library.  Assumes that the public library is
-- non-Backpack.
cabalTestsPackages :: LocalBuildInfo -> [(OpenUnitId, ModuleRenaming)]
cabalTestsPackages lbi =
    case componentNameCLBIs lbi (CExeName (mkUnqualComponentName "test-runtime-deps")) of
        [clbi] -> -- [ (unUnitId $ unDefUnitId duid,rn) | (DefiniteUnitId duid, rn) <- componentIncludes clbi ]
                  componentIncludes clbi
        _ -> error "cabalTestsPackages"
