module UnitTests.Distribution.Client.Targets (
  tests
  ) where

import Distribution.Client.Targets     (UserQualifier(..), UserConstraint(..)
                                       ,readUserConstraint)
import Distribution.Compat.ReadP       (readP_to_S)
import Distribution.Package            (mkPackageName)
import Distribution.PackageDescription (mkFlagName)
import Distribution.Version            (anyVersion, thisVersion, mkVersion)
import Distribution.ParseUtils         (parseCommaList)
import Distribution.Text               (parse)

import Distribution.Solver.Types.PackageConstraint (PackageProperty(..))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Char                       (isSpace)
import Data.List                       (intercalate)

-- Helper function: makes a test group by mapping each element
-- of a list to a test case.
makeGroup :: String -> (a -> Assertion) -> [a] -> TestTree
makeGroup name f xs = testGroup name $
                      zipWith testCase (map show [0 :: Integer ..]) (map f xs)

tests :: [TestTree]
tests =
  [ makeGroup "readUserConstraint" (uncurry readUserConstraintTest)
      exampleConstraints
    
  , makeGroup "parseUserConstraint" (uncurry parseUserConstraintTest)
      exampleConstraints
  
  , makeGroup "readUserConstraints" (uncurry readUserConstraintsTest)
      [-- First example only.
       (head exampleStrs, take 1 exampleUcs),
       -- All examples separated by commas.
       (intercalate ", " exampleStrs, exampleUcs)]
  ]
  where
    (exampleStrs, exampleUcs) = unzip exampleConstraints

exampleConstraints :: [(String, UserConstraint)]
exampleConstraints =
  [ ("template-haskell installed",
     UserConstraint UserToplevel (pn "template-haskell")
                    PackagePropertyInstalled)
    
  , ("bytestring -any",
     UserConstraint UserToplevel (pn "bytestring")
                    (PackagePropertyVersion anyVersion))
  
  , ("process:setup.bytestring ==5.2",
     UserConstraint (UserSetup (pn "process")) (pn "bytestring")
                    (PackagePropertyVersion (thisVersion (mkVersion [5, 2]))))
    
  , ("network:setup.containers +foo -bar baz",
     UserConstraint (UserSetup (pn "network")) (pn "containers")
                    (PackagePropertyFlags [(fn "foo", True),
                                           (fn "bar", False),
                                           (fn "baz", True)]))
    
  -- -- TODO: Re-enable UserExe tests once we decide on a syntax.
  --
  -- , ("foo:happy:exe.template-haskell test",
  --    UserConstraint (UserExe (pn "foo") (pn "happy")) (pn "template-haskell")
  --                   (PackagePropertyStanzas [TestStanzas]))
  ]
  where
    pn = mkPackageName
    fn = mkFlagName

readUserConstraintTest :: String -> UserConstraint -> Assertion
readUserConstraintTest str uc =
  assertEqual ("Couldn't read constraint: '" ++ str ++ "'") expected actual
  where
    expected = uc
    actual   = let Right r = readUserConstraint str in r

parseUserConstraintTest :: String -> UserConstraint -> Assertion
parseUserConstraintTest str uc =
  assertEqual ("Couldn't parse constraint: '" ++ str ++ "'") expected actual
  where
    expected = [uc]
    actual   = [ x | (x, ys) <- readP_to_S parse str
                   , all isSpace ys]

readUserConstraintsTest :: String -> [UserConstraint] -> Assertion
readUserConstraintsTest str ucs =
  assertEqual ("Couldn't read constraints: '" ++ str ++ "'") expected actual
  where
    expected = [ucs]
    actual   = [ x | (x, ys) <- readP_to_S (parseCommaList parse) str
                   , all isSpace ys]
