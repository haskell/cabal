module UnitTests.Distribution.Client.Targets (
  tests
  ) where

import Distribution.Client.Targets     (UserConstraint (..), readUserConstraint)
import Distribution.Compat.ReadP       (ReadP, readP_to_S)
import Distribution.Package            (PackageName (..))
import Distribution.ParseUtils         (parseCommaList)
import Distribution.Text               (parse)

import Test.Framework                  as TF (Test)
import Test.Framework.Providers.HUnit  (testCase)
import Test.HUnit                      (Assertion, assertEqual)

import Data.Char                       (isSpace)

tests :: [TF.Test]
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

    expected = UserConstraintInstalled (PackageName pkgName)
    actual   = let (Right r) = readUserConstraint constr in r

parseUserConstraintTest :: Assertion
parseUserConstraintTest =
  assertEqual ("Couldn't parse constraint: '" ++ constr ++ "'") expected actual
  where
    pkgName  = "template-haskell"
    constr   = pkgName ++ " installed"

    expected = [UserConstraintInstalled (PackageName pkgName)]
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

    expected = [[UserConstraintInstalled (PackageName pkgName)]]
    actual   = [ x | (x, ys) <- readP_to_S parseUserConstraints constr
                   , all isSpace ys]

    parseUserConstraints :: ReadP r [UserConstraint]
    parseUserConstraints = parseCommaList parse
