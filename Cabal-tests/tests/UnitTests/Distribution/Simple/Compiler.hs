module UnitTests.Distribution.Simple.Compiler
  ( tests
  ) where

import Control.Exception qualified as C
import Data.List (isInfixOf)
import Data.Map (fromList)
import Distribution.Compiler
import Distribution.Package
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Compiler
import Distribution.Version
import Language.Haskell.Extension
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read (readMaybe)

tests :: [TestTree]
tests =
  [ testGroup "flagToDebugInfoLevel" flagToDebugInfoLevelTests
  , testGroup "flagToOptimisationLevel" flagToOptimisationLevelTests
  , testGroup "DebugInfoLevel Parsec" debugInfoLevelParsecTests
  , testGroup "OptimisationLevel Parsec" optimisationLevelParsecTests
  , testGroup "Show/Read instance" showReadTests
  ]

flagToDebugInfoLevelTests :: [TestTree]
flagToDebugInfoLevelTests =
  [ testCase "defaults to NormalDebugInfo" $
      flagToDebugInfoLevel Nothing @?= NormalDebugInfo
  , testCase "--enable-debug-info=True" $
      flagToDebugInfoLevel (Just "True") @?= NormalDebugInfo
  , testCase "--enable-debug-info=False" $
      flagToDebugInfoLevel (Just "False") @?= NoDebugInfo
  , testCase "--enable-debug-info=0" $
      flagToDebugInfoLevel (Just "0") @?= NoDebugInfo
  , testCase "--enable-debug-info=1" $
      flagToDebugInfoLevel (Just "1") @?= MinimalDebugInfo
  , testCase "--enable-debug-info=2" $
      flagToDebugInfoLevel (Just "2") @?= NormalDebugInfo
  , testCase "--enable-debug-info=3" $
      flagToDebugInfoLevel (Just "3") @?= MaximalDebugInfo
  , testCase "--enable-debug-info=4 is out of range" $
      assertError "expected an error for out-of-range debug info level" $
        flagToDebugInfoLevel (Just "4")
  , testCase "--enable-debug-info=foo is unparsable" $
      assertError "expected an error for unparsable debug info level" $
        flagToDebugInfoLevel (Just "foo")
  ]

flagToOptimisationLevelTests :: [TestTree]
flagToOptimisationLevelTests =
  [ testCase "defaults to NormalOptimisation" $
      flagToOptimisationLevel Nothing @?= NormalOptimisation
  , testCase "--enable-optimization=True" $
      flagToOptimisationLevel (Just "True") @?= NormalOptimisation
  , testCase "--enable-optimization=False" $
      flagToOptimisationLevel (Just "False") @?= NoOptimisation
  , testCase "--enable-optimization=0" $
      flagToOptimisationLevel (Just "0") @?= NoOptimisation
  , testCase "--enable-optimization=1" $
      flagToOptimisationLevel (Just "1") @?= NormalOptimisation
  , testCase "--enable-optimization=2" $
      flagToOptimisationLevel (Just "2") @?= MaximumOptimisation
  , testCase "--enable-optimization=3 is out of range" $
      assertError "expected an error for out-of-range optimisation level" $
        flagToOptimisationLevel (Just "3")
  , testCase "--enable-optimization=foo is unparsable" $
      assertError "expected an error for unparsable optimisation level" $
        flagToOptimisationLevel (Just "foo")
  ]

debugInfoLevelParsecTests :: [TestTree]
debugInfoLevelParsecTests =
  [ testCase "parses True" $
      eitherParsec "True" @?= (Right NormalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses False" $
      eitherParsec "False" @?= (Right NoDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 0" $
      eitherParsec "0" @?= (Right NoDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 1" $
      eitherParsec "1" @?= (Right MinimalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 2" $
      eitherParsec "2" @?= (Right NormalDebugInfo :: Either String DebugInfoLevel)
  , testCase "parses 3" $
      eitherParsec "3" @?= (Right MaximalDebugInfo :: Either String DebugInfoLevel)
  ]

optimisationLevelParsecTests :: [TestTree]
optimisationLevelParsecTests =
  [ testCase "parses True" $
      eitherParsec "True" @?= (Right NormalOptimisation :: Either String OptimisationLevel)
  , testCase "parses False" $
      eitherParsec "False" @?= (Right NoOptimisation :: Either String OptimisationLevel)
  , testCase "parses 0" $
      eitherParsec "0" @?= (Right NoOptimisation :: Either String OptimisationLevel)
  , testCase "parses 1" $
      eitherParsec "1" @?= (Right NormalOptimisation :: Either String OptimisationLevel)
  , testCase "parses 2" $
      eitherParsec "2" @?= (Right MaximumOptimisation :: Either String OptimisationLevel)
  ]

-- | A 'Compiler' as configured for a recent GHC, with 'compilerWiredInUnitIds'
-- populated (GHC >= 9.14) or not (older compilers).
testCompiler :: Maybe [(PackageName, UnitId)] -> Compiler
testCompiler wired =
  Compiler
    { compilerId = CompilerId GHC (mkVersion [9, 14, 1])
    , compilerAbiTag = NoAbiTag
    , compilerCompat = []
    , compilerLanguages = [(Haskell2010, "-XHaskell2010")]
    , compilerExtensions =
        [ (EnableExtension OverloadedStrings, Just "-XOverloadedStrings")
        ]
    , compilerProperties = fromList [("Have interpreter", "YES")]
    , compilerWiredInUnitIds = wired
    }

-- | The wired-in unit ids of a GHC >= 9.14 compiler.
wiredInUnitIds :: [(PackageName, UnitId)]
wiredInUnitIds =
  [ (mkPackageName "ghc", mkUnitId "ghc-9.14.1")
  , (mkPackageName "ghc-internal", mkUnitId "ghc-internal-9.14.1")
  ]

showReadTests :: [TestTree]
showReadTests =
  [ testCase "show renders valid Haskell source" $ do
      let shown = show (testCompiler (Just wiredInUnitIds))
      assertBool ("uses mkPackageName: " ++ shown) $
        "(mkPackageName \"" `isInfixOf` shown
      assertBool ("uses mkUnitId: " ++ shown) $
        ",mkUnitId \"" `isInfixOf` shown
      assertBool ("does not use the PackageName constructor: " ++ shown) $
        not ("(PackageName \"" `isInfixOf` shown)
      assertBool ("does not use the UnitId constructor: " ++ shown) $
        not (",UnitId \"" `isInfixOf` shown)
  , testCase "read . show round-trips with wired-in unit ids" $
      assertEqual "round-trip" (Just (testCompiler (Just wiredInUnitIds))) $
        readMaybe (show (testCompiler (Just wiredInUnitIds)))
  , testCase "read . show round-trips without wired-in unit ids" $
      assertEqual "round-trip" (Just (testCompiler Nothing)) $
        readMaybe (show (testCompiler Nothing))
  , testCase "readsPrec parses parenthesised values at high precedence" $
      assertEqual
        "parenthesised"
        [(testCompiler (Just wiredInUnitIds), "")]
        (readsPrec 11 ("(" ++ show (testCompiler (Just wiredInUnitIds)) ++ ")"))
  , testCase "readsPrec rejects non-parenthesised values at high precedence" $
      assertEqual
        "non-parenthesised"
        ([] :: [(Compiler, String)])
        (readsPrec 11 (show (testCompiler (Just wiredInUnitIds))))
  ]

assertError :: String -> a -> Assertion
assertError msg x =
  C.catch
    (C.evaluate x >> assertFailure msg)
    (\(C.ErrorCall _) -> return ())
