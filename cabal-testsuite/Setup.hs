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
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Verbosity (Verbosity, defaultVerbosityHandles, mkVerbosity, normal)

import Data.List (intercalate, isPrefixOf)
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { confHook = \args flags -> do
        lbi <- confHook simpleUserHooks args flags
        generateScriptEnvModule lbi
            $ mkVerbosity defaultVerbosityHandles
            $ fromFlagOrDefault normal (configVerbosity flags)
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
      , "lbiPackageDbStack = " ++ renderPackageDBStack lbiPackageDbStack
      , ""
      , "lbiPlatform :: Platform"
      , "lbiPlatform = " ++ show (hostPlatform lbi)
      , ""
      , "lbiCompiler :: Compiler"
      -- We added a new field to compiler so we need to be careful
      -- to make sure that it is always defined,
      -- even if the test suite is being built with an older Cabal
#if MIN_VERSION_Cabal(3,15,0)
      , "lbiCompiler = " ++ fixupCompilerShow (show (compiler lbi))
#else
      , "lbiCompiler = " ++ init (show (compiler lbi)) ++ ", compilerWiredInUnitIds = Nothing}"
#endif
      , ""
      , "lbiPackages :: [(OpenUnitId, ModuleRenaming)]"
      , "lbiPackages = read " ++ show (show (cabalTestsPackages lbi))
      , ""
      , "lbiProgramDb :: ProgramDb"
      , "lbiProgramDb = read " ++ show (show (withPrograms lbi))
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
    libAutogenDir = getSymbolicPath (autogenPackageModulesDir lbi)

-- | Convert package database into absolute path, so that
-- if we change working directories in a subprocess we get the correct database.
canonicalizePackageDB :: PackageDB -> IO PackageDB
canonicalizePackageDB (SpecificPackageDB path)
    = SpecificPackageDB . makeSymbolicPath <$> canonicalizePath (getSymbolicPath path)
canonicalizePackageDB x = return x

-- | Render a package database stack as Haskell source code.
--
-- 'PackageDBStackCWD' holds plain 'FilePath's (unlike the 'PackageDB' stack in
-- 'LocalBuildInfo', whose paths are 'SymbolicPath's with an abstract
-- constructor, so we cannot use 'show' on it).
renderPackageDBStack :: PackageDBStack -> String
renderPackageDBStack pdbs = "[" ++ intercalate "," (map renderPackageDB pdbs) ++ "]"

renderPackageDB :: PackageDB -> String
renderPackageDB GlobalPackageDB = "GlobalPackageDB"
renderPackageDB UserPackageDB = "UserPackageDB"
renderPackageDB (SpecificPackageDB path) = "SpecificPackageDB " ++ show (getSymbolicPath path)

-- | The 'Show' instance of 'Compiler' renders wired-in unit ids as
-- @PackageName \"ghc@ and @UnitId \"ghc-9.14.1\"@, but neither constructor is
-- exported (GHC >= 9.14 populates 'compilerWiredInUnitIds', older compilers
-- yield 'Nothing'), so the raw output is not valid source code. Use the
-- exported 'mkPackageName' / 'mkUnitId' instead.
fixupCompilerShow :: String -> String
fixupCompilerShow = go
  where
    fixes =
      [ ("PackageName \"", "mkPackageName \"")
      , ("UnitId \"", "mkUnitId \"")
      ]
    go s = case [ (repl, drop (length needle) s) | (needle, repl) <- fixes, needle `isPrefixOf` s ] of
      ((repl, rest) : _) -> repl ++ go rest
      [] -> case s of
        (c : cs) -> c : go cs
        [] -> []

-- | Compute the set of @-package-id@ flags which would be passed when
-- building the public library.  Assumes that the public library is
-- non-Backpack.
cabalTestsPackages :: LocalBuildInfo -> [(OpenUnitId, ModuleRenaming)]
cabalTestsPackages lbi =
    case componentNameCLBIs lbi (CExeName (mkUnqualComponentName "test-runtime-deps")) of
        [clbi] -> -- [ (unUnitId $ unDefUnitId duid,rn) | (DefiniteUnitId duid, rn) <- componentIncludes clbi ]
                  componentIncludes clbi
        _ -> error "cabalTestsPackages"
