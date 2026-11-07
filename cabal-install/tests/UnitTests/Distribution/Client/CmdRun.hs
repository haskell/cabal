module UnitTests.Distribution.Client.CmdRun
  ( tests
  ) where

import Distribution.Client.CmdRun
  ( RunProblem (..)
  , renderRunProblem
  )
import Distribution.Client.ProjectPlanning
  ( AvailableTarget (..)
  , AvailableTargetStatus (..)
  , TargetRequested (..)
  )
import Distribution.Client.TargetSelector
  ( TargetSelector (..)
  )

import Distribution.Package (PackageIdentifier (..), PackageName, mkPackageName)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Distribution.Version (mkVersion)

import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "hints at each executable when there are multiple" testMultipleExecutablesHint
  , testCase "hint qualifies test suites and benchmarks" testHintQualifiesNonExeComponents
  , testCase "hint without executables" testHintWithoutExecutables
  ]

pkgname :: PackageName
pkgname = mkPackageName "agent-cli"

pkgid :: PackageIdentifier
pkgid = PackageIdentifier pkgname (mkVersion [0, 1, 0, 0])

mkTarget :: ComponentName -> AvailableTarget ()
mkTarget cname =
  AvailableTarget
    { availableTargetPackageId = pkgid
    , availableTargetComponentName = cname
    , availableTargetStatus = TargetBuildable () TargetRequestedByDefault
    , availableTargetLocalToProject = True
    }

exe :: String -> AvailableTarget ()
exe = mkTarget . CExeName . mkUnqualComponentName

test :: String -> AvailableTarget ()
test = mkTarget . CTestName . mkUnqualComponentName

bench :: String -> AvailableTarget ()
bench = mkTarget . CBenchName . mkUnqualComponentName

selector :: TargetSelector
selector = TargetPackageNamed pkgname Nothing

testMultipleExecutablesHint :: Assertion
testMultipleExecutablesHint = do
  let rendered =
        renderRunProblem $
          TargetProblemMatchesMultiple
            selector
            ( exe "agent-cli"
                :| [ exe "agent-telegram"
                   , exe "eval-ghci-vs-bash"
                   ]
            )
  assertBool "hint header is present" $
    "You need to specify which component cabal should use. Try one of those:"
      `isInfixOf` rendered
  mapM_ (assertSuggestion rendered) ["agent-cli", "agent-telegram", "eval-ghci-vs-bash"]

testHintQualifiesNonExeComponents :: Assertion
testHintQualifiesNonExeComponents = do
  let rendered =
        renderRunProblem $
          TargetProblemMatchesMultiple
            selector
            ( exe "agent-cli"
                :| [ exe "agent-telegram"
                   , test "agent-cli-test"
                   , bench "image-preview-latency-bench"
                   ]
            )
  mapM_ (assertSuggestion rendered) ["agent-cli", "agent-telegram"]
  assertBool "test suite is suggested with a test: qualifier" $
    "- cabal run agent-cli:test:agent-cli-test" `isInfixOf` rendered
  assertBool "benchmark is suggested with a bench: qualifier" $
    "- cabal run agent-cli:bench:image-preview-latency-bench" `isInfixOf` rendered

testHintWithoutExecutables :: Assertion
testHintWithoutExecutables = do
  let rendered =
        renderRunProblem $
          TargetProblemMatchesMultiple
            selector
            ( test "agent-cli-test"
                :| [bench "image-preview-latency-bench"]
            )
  assertBool "hint header is present" $
    "You need to specify which component cabal should use. Try one of those:"
      `isInfixOf` rendered
  assertBool "test suite is labeled as test-suite" $
    "- test-suites: agent-cli-test" `isInfixOf` rendered
  assertBool "benchmark is labeled as benchmark" $
    "- benchmarks: image-preview-latency-bench" `isInfixOf` rendered
  assertBool "no executables section" $
    not ("- executables:" `isInfixOf` rendered)
  assertBool "test suite is suggested with a test: qualifier" $
    "- cabal run agent-cli:test:agent-cli-test" `isInfixOf` rendered
  assertBool "benchmark is suggested with a bench: qualifier" $
    "- cabal run agent-cli:bench:image-preview-latency-bench" `isInfixOf` rendered

assertSuggestion :: String -> String -> Assertion
assertSuggestion rendered exeName =
  assertBool ("suggestion for executable " ++ exeName) $
    ("- cabal run agent-cli:" ++ exeName) `isInfixOf` rendered
