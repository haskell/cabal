{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Simple.GHC (tests) where

import qualified Data.Map as Map
import Control.Monad (forM_, void)
import Data.List (nubBy)
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Simple.Compiler
import qualified Distribution.Simple.GHC as GHC
import Distribution.Simple.Program
import Distribution.Simple.Utils (ordNub)
import Distribution.Utils.Path (getSymbolicPath, makeSymbolicPath)
import Distribution.Verbosity (defaultVerbosityHandles, mkVerbosity, silent)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Cabal ()

tests :: FilePath -> TestTree
tests ghcPath = testGroup "Distribution.Simple.GHC"
    [
        testProperty "linker probe uses the configured package database stack" $
#if MIN_VERSION_QuickCheck(2,18,0)
            withNumTests 10 $
#else
            withMaxSuccess 10 $
#endif
              compilerProbePackageDBsProp ghcPath
    ]

-- Each database gets a distinct package so that the probe must see the whole
-- stack. Paths are valid relative directory names and may contain spaces.
data ProbePackageDBs = ProbePackageDBs
    { probePackageDBs :: PackageDBStack
    , probePackages   :: NonEmptyList (PackageName, FilePath)
    }
    deriving (Eq, Show)

instance Arbitrary ProbePackageDBs where
    arbitrary = do
        prefix <- elements
            [ [GlobalPackageDB]
            , [GlobalPackageDB, UserPackageDB]
            ]
        packages <- resize 3 $ listOf1 $
                ((,) . mkPackageName . ("probe-" ++) . unPackageName <$> arbitrary)
            <*> packageDbPath
        missingPaths <- resize 3 $ listOf packageDbPath
        databases <- shuffle $
            map (SpecificPackageDB . makeSymbolicPath) (map snd packages ++ missingPaths)
        return $ fixProbePackageDBs $ ProbePackageDBs (prefix ++ databases) (NonEmpty packages)
      where
        packageDbPath = ("db-" ++) . unwords <$> resize 3
            (listOf1 $ listOf1 $ elements ['a' .. 'z'])

    shrink input@(ProbePackageDBs databases (NonEmpty packages)) =
        filter ((< size input) . size) $ map fixProbePackageDBs $
            [ ProbePackageDBs databases' (NonEmpty packages)
            | databases' <- shrinkList (const []) databases
            ] ++
            [ ProbePackageDBs databases (NonEmpty packages')
            | packages' <- shrinkList (const []) packages
            , not (null packages')
            ]
      where
        size (ProbePackageDBs dbs (NonEmpty pkgs)) = (length pkgs, length dbs)

-- Keep both names and paths distinct, and retain every package's database in
-- the stack. The remaining specific databases are intentionally left missing.
fixProbePackageDBs :: ProbePackageDBs -> ProbePackageDBs
fixProbePackageDBs (ProbePackageDBs databases (NonEmpty packages)) =
    ProbePackageDBs
        (    [GlobalPackageDB]
          ++ [UserPackageDB | UserPackageDB `elem` databases]
          ++ (SpecificPackageDB . makeSymbolicPath) `map` paths
        )
        (NonEmpty distinctPackages)
  where
    distinctPackages = nubBy coincides packages
    coincides (name, path) (name', path') = name == name' || path == path'
    paths = ordNub $ [getSymbolicPath path | SpecificPackageDB path <- databases]
                  ++ snd `map` distinctPackages

compilerProbePackageDBsProp :: FilePath -> ProbePackageDBs -> Property
compilerProbePackageDBsProp ghcPath (ProbePackageDBs databases (NonEmpty packages)) = ioProperty $
    withSystemTempDirectory "cabal-probe-package-dbs" $ \root -> do
        let verbosity = mkVerbosity defaultVerbosityHandles silent
        (compiler, _, ghcDb) <- GHC.configureCompiler verbosity (Just ghcPath) defaultProgramDb
        (ghc, _) <- requireProgram verbosity ghcProgram ghcDb
        let packageDbs :: PackageDBStackCWD
            packageDbs = interpretPackageDBStack (Just $ makeSymbolicPath root) databases

            probeGhc :: ConfiguredProgram
            probeGhc = ghc
                { -- Exercise the linker probe even when the test compiler can
                  -- normally use --merge-objs and skip it.
                  programProperties = Map.insert "Supports --merge-objs" "NO" (programProperties ghc)
                , programOverrideArgs = programOverrideArgs ghc
                    ++ map (("-plugin-package=" ++) . unPackageName . fst) packages
                }

        programDb <- GHC.compilerProgramDbWithPackageDBs packageDbs verbosity compiler
            (updateProgram probeGhc ghcDb) Nothing
        (ghcPkg, _) <- requireProgram verbosity ghcPkgProgram programDb
        forM_ packages $ \(packageName, path) -> do
            let name = unPackageName packageName
                db = root </> path
            createDirectory db
            writeFile (db </> name ++ ".conf") $ unlines
                [ "name: " ++ name
                , "version: 0"
                , "id: " ++ name ++ "-0"
                , "key: " ++ name ++ "-0"
                , "exposed: True"
                ]
            runProgram verbosity ghcPkg ["--package-db", db, "recache"]
        case lookupKnownProgram "ld" programDb of
          Just linker -> void $ requireProgram verbosity linker programDb
          Nothing -> fail "GHC toolchain did not register ld"
        return True
