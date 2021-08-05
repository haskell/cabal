module UnitTests.Distribution.Client.Init
  ( tests
  ) where

import Distribution.Client.Init.FileCreators
  ( generateCabalFile )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (goldenVsString)

import System.FilePath
  ( (</>) )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import Distribution.Client.Init.Command
  ( getLibOrExec, getAppDir, getSrcDir )
import Distribution.Client.Init.Types
  ( InitFlags(..), PackageType(..), defaultInitFlags )
import Distribution.Simple.Setup
  ( Flag(..) )

import Distribution.CabalSpecVersion
  ( CabalSpecVersion(CabalSpecV2_4) )
import Distribution.Types.Dependency
  ( Dependency, mkDependency, mainLibSet )
import Distribution.Types.PackageName
  ( mkPackageName )
import Distribution.Types.VersionRange
  ( majorBoundVersion )
import Distribution.Types.Version
  ( mkVersion )
import qualified Distribution.ModuleName as ModuleName
  ( fromString )
import qualified Distribution.SPDX as SPDX
import Language.Haskell.Extension ( Language(..) )

tests :: [TestTree]
tests = [ testGroup "cabal init goldens"
          [ checkCabalFileGolden exeFlags "exe-only-golden.cabal"
          , checkCabalFileGolden libAndExeFlags "lib-and-exe-golden.cabal"
          , checkCabalFileGolden libExeAndTestFlags "lib-exe-and-test-golden.cabal"
          , checkCabalFileGolden libExeAndTestWithCommentsFlags "lib-exe-and-test-with-comments-golden.cabal"
          ]
        , testGroup "Check init flag outputs against init script builds"
          [ checkInitFlags "Check library-only build flags"  libFlags Library
          , checkInitFlags "Check lib+exe build flags" libAndExeFlags LibraryAndExecutable
          , checkInitFlags "Check exe-only build flags" exeFlags Executable
          ]
        ]

checkCabalFileGolden :: InitFlags -> FilePath -> TestTree
checkCabalFileGolden flags goldenFileName =
  goldenVsString goldenFileName goldenFilePath generatedCabalFile
  where
    goldenFilePath :: FilePath
    goldenFilePath = "tests" </> "fixtures" </> "init" </> goldenFileName

    generatedCabalFile :: IO BS.ByteString
    generatedCabalFile = pure . BS8.pack $ generateCabalFile goldenFileName flags

checkInitFlags :: String -> InitFlags -> PackageType -> TestTree
checkInitFlags label flags pkgType = testCase label $ do
    flags' <- getLibOrExec rawFlags
      >>= getAppDir
      >>= getSrcDir

    flags @=? flags'
 where
   rawFlags
     | pkgType == Executable = baseFlags
       { packageType = Flag pkgType
       , exposedModules = Nothing
       }
     | otherwise = baseFlags { packageType = Flag pkgType }


-- ==================================================
-- Base flags to set common InitFlags values.

baseFlags :: InitFlags
baseFlags = defaultInitFlags {
  -- Values common to all (or most) test flags.
    packageName = Flag (mkPackageName "foo")
  , noComments = Flag False
  , minimal = Flag True
  , version = Flag (mkVersion [3,2,1])
  , synopsis = Flag "The foo package"
  , homepage = Flag "https://github.com/foo/foo"
  , license = Flag SPDX.NONE
  , author = Flag "me"
  , email = Flag "me@me.me"
  , category = Flag (Left "SomeCat")
  , cabalVersion = Flag CabalSpecV2_4
  , extraSrc = Just ["CHANGELOG.md"]
  , interactive = Flag False
  , otherModules = Nothing
  , otherExts = Nothing
  , language = Flag Haskell2010
  , buildTools = Nothing
  , dependencies = Just testDependencies
  , quiet = Flag True
  , packageDir = NoFlag
  , simpleProject = Flag False
  , initHcPath = NoFlag
  , overwrite = NoFlag

  -- Commonly overridden values in test InitFlags.
  -- It is fine to provide the same value in an overridden InitFlags
  -- to make it clear what that particular test case is differentiating
  -- from others.
  , packageType = Flag Executable
  , mainIs = Flag "Main.hs"
  , applicationDirs = Just ["app"]
  , sourceDirs = Nothing
  , exposedModules = Just [ModuleName.fromString "MyLib"]
  , initializeTestSuite = Flag False
  , testDirs = Nothing
  }


-- ==================================================
-- Simple library flags

libFlags :: InitFlags
libFlags = baseFlags
  { packageType = Flag Library
  , mainIs = NoFlag
  , sourceDirs = Just ["src"]
  , applicationDirs = Just []
  }

-- ==================================================
-- Simple exe.

exeFlags :: InitFlags
exeFlags = baseFlags {
  -- Create an executable only, with main living in app/Main.hs.
    packageType = Flag Executable
  , mainIs = Flag "Main.hs"
  , sourceDirs = Just []
  , applicationDirs = Just ["app"]
  , exposedModules = Nothing
  }


-- ==================================================
-- Simple lib and exe (as created by `cabal init --libandexe`).
--
-- Specifically, having 'exposedModules = Just ["MyLib"]' is a special
-- case which results in the executable depending on the library from
-- the same package, i.e. 'build-depends = foo' with no version
-- constraints.

libAndExeFlags :: InitFlags
libAndExeFlags = baseFlags {
  -- Create a library and executable
    packageType = Flag LibraryAndExecutable

  -- Main living in app/Main.hs.
  , mainIs = Flag "Main.hs"
  , applicationDirs = Just ["app"]

  -- Library sources live in src/ and expose the module MyLib.
  , sourceDirs = Just ["src"]
  }


-- ==================================================
-- Lib, exe, and test suite

libExeAndTestFlags :: InitFlags
libExeAndTestFlags = baseFlags {
  -- Create a library and executable
    packageType = Flag LibraryAndExecutable

  -- Main living in app/Main.hs.
  , mainIs = Flag "Main.hs"
  , applicationDirs = Just ["app"]

  -- Library sources live in src/ and expose the modules A and B.
  , sourceDirs = Just ["src"]
  , exposedModules = Just (map ModuleName.fromString ["A", "B"])

  -- Create a test suite living in tests/
  , initializeTestSuite = Flag True
  , testDirs = Just ["tests"]
  }

-- ==================================================
-- Lib, exe, and test suite with comments.

libExeAndTestWithCommentsFlags :: InitFlags
libExeAndTestWithCommentsFlags = libExeAndTestFlags {
    minimal = Flag False
  , noComments = Flag False
  , quiet = Flag False
  }



-- ==================================================
-- Test dependency.

testDependencies :: [Dependency]
testDependencies =
  [ mkDependency
      (mkPackageName "base")
      (majorBoundVersion (mkVersion [4,13,0,0]))
      mainLibSet
  , mkDependency
      (mkPackageName "containers")
      (majorBoundVersion (mkVersion [5,7,0,0]))
      mainLibSet
  , mkDependency
      (mkPackageName "unordered-containers")
      (majorBoundVersion (mkVersion [2,7,0,0]))
      mainLibSet
  ]
