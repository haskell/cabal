-- The intention is that this will be the new unit test framework.
-- Please add any working tests here.  This file should do nothing
-- but import tests from other modules.
--
-- Stephen Blackheath, 2009

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import qualified Test.HUnit as HUnit
import PackageTests.BuildDeps.SameDepsAllRound.Check
import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check
import PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check
import PackageTests.BuildDeps.InternalLibrary0.Check
import Distribution.Text (display)
import Distribution.Simple.Utils (cabalVersion)
import Data.Version

hunit :: TestName -> HUnit.Test -> Test
hunit name test = testGroup name $ hUnitTestToTests test

tests :: Version -> [Test]
tests cabalVersion = [
        hunit "PackageTests/BuildDeps/SameDepsAllRound/" PackageTests.BuildDeps.SameDepsAllRound.Check.suite,
        hunit "PackageTests/BuildDeps/GlobalBuildDepsNotAdditive1/" PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check.suite,
        hunit "PackageTests/BuildDeps/GlobalBuildDepsNotAdditive2/" PackageTests.BuildDeps.GlobalBuildDepsNotAdditive2.Check.suite,
        hunit "PackageTests/BuildDeps/InternalLibrary0/" (PackageTests.BuildDeps.InternalLibrary0.Check.suite cabalVersion)
    ]

main = do
    putStrLn $ "Cabal test suite - testing cabal version "++display cabalVersion
    defaultMain (tests cabalVersion)

