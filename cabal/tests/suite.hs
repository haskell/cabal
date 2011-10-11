-- The intention is that this will be the new unit test framework.
-- Please add any working tests here.  This file should do nothing
-- but import tests from other modules.
--
-- Stephen Blackheath, 2009

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as HUnit
import PackageTests.BenchmarkStanza.Check
import PackageTests.BuildDeps.SameDepsAllRound.Check
import PackageTests.BuildDeps.TargetSpecificDeps1.Check
import PackageTests.BuildDeps.TargetSpecificDeps1.Check
import PackageTests.BuildDeps.TargetSpecificDeps2.Check
import PackageTests.BuildDeps.TargetSpecificDeps3.Check
import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check
import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check
import PackageTests.BuildDeps.InternalLibrary0.Check
import PackageTests.BuildDeps.InternalLibrary1.Check
import PackageTests.BuildDeps.InternalLibrary2.Check
import PackageTests.BuildDeps.InternalLibrary3.Check
import PackageTests.BuildDeps.InternalLibrary4.Check
import PackageTests.TestOptions.Check
import PackageTests.TestStanza.Check
import PackageTests.TestSuiteExeV10.Check
import Distribution.Text (display)
import Distribution.Simple.Utils (cabalVersion)
import Data.Version
import System.Directory

hunit :: TestName -> HUnit.Test -> Test
hunit name test = testGroup name $ hUnitTestToTests test

tests :: Version -> [Test]
tests cabalVersion = [
        hunit "PackageTests/BuildDeps/SameDepsAllRound/" PackageTests.BuildDeps.SameDepsAllRound.Check.suite,
        hunit "PackageTests/BuildDeps/GlobalBuildDepsNotAdditive1/" PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite,
        hunit "PackageTests/BuildDeps/GlobalBuildDepsNotAdditive2/" PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite,
        hunit "PackageTests/BuildDeps/InternalLibrary0/" (PackageTests.BuildDeps.InternalLibrary0.Check.suite cabalVersion),
        hunit "PackageTests/TestStanza/" (PackageTests.TestStanza.Check.suite cabalVersion),
        -- ^ The Test stanza test will eventually be required
        -- only for higher versions.
        hunit "PackageTests/TestSuiteExeV10/Test"
        (PackageTests.TestSuiteExeV10.Check.checkTest cabalVersion),
        hunit "PackageTests/TestSuiteExeV10/TestWithHpc"
        (PackageTests.TestSuiteExeV10.Check.checkTestWithHpc cabalVersion),
        hunit "PackageTests/TestOptions" PackageTests.TestOptions.Check.suite,
        hunit "PackageTests/BenchmarkStanza/" (PackageTests.BenchmarkStanza.Check.suite cabalVersion)
        -- ^ The benchmark stanza test will eventually be required
        -- only for higher versions.
    ] ++
    -- These tests are only required to pass on cabal version >= 1.7
    (if cabalVersion >= Version [1, 7] []
        then [
            hunit "PackageTests/BuildDeps/TargetSpecificDeps1/" PackageTests.BuildDeps.TargetSpecificDeps1.Check.suite,
            hunit "PackageTests/BuildDeps/TargetSpecificDeps2/" PackageTests.BuildDeps.TargetSpecificDeps2.Check.suite,
            hunit "PackageTests/BuildDeps/TargetSpecificDeps3/" PackageTests.BuildDeps.TargetSpecificDeps3.Check.suite,
            hunit "PackageTests/BuildDeps/InternalLibrary1/" PackageTests.BuildDeps.InternalLibrary1.Check.suite,
            hunit "PackageTests/BuildDeps/InternalLibrary2/" PackageTests.BuildDeps.InternalLibrary2.Check.suite,
            hunit "PackageTests/BuildDeps/InternalLibrary3/" PackageTests.BuildDeps.InternalLibrary3.Check.suite,
            hunit "PackageTests/BuildDeps/InternalLibrary4/" PackageTests.BuildDeps.InternalLibrary4.Check.suite
        ]
        else [])

main = do
    putStrLn $ "Cabal test suite - testing cabal version "++display cabalVersion
    setCurrentDirectory "tests"
    defaultMain (tests cabalVersion)

