{-# LANGUAGE Haskell2010 #-}
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
import Distribution.Verbosity

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
      [ "module Test.Cabal.ScriptEnv0 where"
      , ""
      , "import Distribution.Simple"
      , "import Distribution.System (Platform(..), Arch(..), OS(..))"
      , "import Distribution.Types.ModuleRenaming"
      , "import Distribution.Simple.Program.Db"
      , "import Distribution.Backpack (OpenUnitId)"
      , "import Data.Map (fromList)"
      , ""
      , "lbiPackageDbStack :: PackageDBStack"
      , "lbiPackageDbStack = " ++ show lbiPackageDbStack
      , ""
      , "lbiPlatform :: Platform"
      , "lbiPlatform = " ++ show (hostPlatform lbi)
      , ""
      , "lbiCompiler :: Compiler"
      , "lbiCompiler = " ++ show (compiler lbi)
      , ""
      , "lbiPackages :: [(OpenUnitId, ModuleRenaming)]"
      , "lbiPackages = read " ++ show (show (cabalTestsPackages lbi))
      , ""
      , "lbiProgramDb :: ProgramDb"
      , "lbiProgramDb = read " ++ show (show (withPrograms lbi))
      , ""
      , "lbiWithSharedLib :: Bool"
      , "lbiWithSharedLib = " ++ show (withSharedLib lbi)
      ]
  where
    moduledir = libAutogenDir </> "Test" </> "Cabal"
    -- fixme: use component-specific folder
    libAutogenDir = autogenPackageModulesDir lbi

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
    case componentNameCLBIs lbi (CExeName (mkUnqualComponentName "cabal-tests")) of
        [clbi] -> -- [ (unUnitId $ unDefUnitId duid,rn) | (DefiniteUnitId duid, rn) <- componentIncludes clbi ]
                  componentIncludes clbi
        _ -> error "cabalTestsPackages"
