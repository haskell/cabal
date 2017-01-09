module UnitTests.Distribution.Client.Targets (
  tests
  ) where

import Distribution.Client.Targets     (UserQualifier(..), UserConstraint(..)
                                       ,readUserConstraint)
import Distribution.Compat.ReadP       (ReadP, readP_to_S)
import Distribution.Package            (mkPackageName)
import Distribution.ParseUtils         (parseCommaList)
import Distribution.Text               (parse)

import Distribution.Solver.Types.PackageConstraint (PackageProperty(..))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Char                       (isSpace)

tests :: [TestTree]
tests = [ testCase "readUserConstraint" readUserConstraintTest
        , testCase "parseUserConstraint" parseUserConstraintTest
        , testCase "readUserConstraints" readUserConstraintsTest
        ]

readUserConstraintTest :: Assertion
readUserConstraintTest =
  assertEqual ("Couldn't read constraint: '" ++ constr ++ "'") expected actual
  where
    pkgName  = "template-haskell"
    constr   = pkgName ++ " installed"

    expected = UserConstraint UserUnqualified (mkPackageName pkgName)
                              PackagePropertyInstalled
    actual   = let (Right r) = readUserConstraint constr in r

parseUserConstraintTest :: Assertion
parseUserConstraintTest =
  assertEqual ("Couldn't parse constraint: '" ++ constr ++ "'") expected actual
  where
    pkgName  = "template-haskell"
    constr   = pkgName ++ " installed"

    expected = [UserConstraint UserUnqualified (mkPackageName pkgName)
                               PackagePropertyInstalled]
    actual   = [ x | (x, ys) <- readP_to_S parseUserConstraint constr
                   , all isSpace ys]

    parseUserConstraint :: ReadP r UserConstraint
    parseUserConstraint = parse

readUserConstraintsTest :: Assertion
readUserConstraintsTest =
  assertEqual ("Couldn't read constraints: '" ++ constr ++ "'") expected actual
  where
    pkgName  = "template-haskell"
    constr   = pkgName ++ " installed"

    expected = [[UserConstraint UserUnqualified (mkPackageName pkgName)
                                PackagePropertyInstalled]]
    actual   = [ x | (x, ys) <- readP_to_S parseUserConstraints constr
                   , all isSpace ys]

    parseUserConstraints :: ReadP r [UserConstraint]
    parseUserConstraints = parseCommaList parse
