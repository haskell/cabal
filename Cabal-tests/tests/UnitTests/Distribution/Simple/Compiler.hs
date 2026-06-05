module UnitTests.Distribution.Simple.Compiler
  ( tests
  ) where

import Data.List (isInfixOf)
import Data.Map (fromList)
import Distribution.Compiler
import Distribution.Package
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Compiler
import Distribution.Types.DebugInfoLevel (DebugInfoLevel (..))
import Distribution.Types.OptimisationLevel (OptimisationLevel (..))
import Distribution.Version
import Language.Haskell.Extension
import Test.Tasty
import Test.Tasty.HUnit
import Text.Read (readMaybe)

tests :: [TestTree]
tests =
  [ testGroup "DebugInfoLevel Parsec" debugInfoLevelParsecTests
  , testGroup "OptimisationLevel Parsec" optimisationLevelParsecTests
  , testGroup "Show/Read instance" showReadTests
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
