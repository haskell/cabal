{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.ProjectPlanning (tests) where

import Data.List.NonEmpty
import Distribution.Client.ProjectPlanning (ComponentTarget (..), SubComponentTarget (..), nubComponentTargets)
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Build Target Tests" buildTargetTests
  ]

-- ----------------------------------------------------------------------------
-- Build Target Tests
-- ----------------------------------------------------------------------------

buildTargetTests :: [TestTree]
buildTargetTests =
  [ testGroup "nubComponentTargets" nubComponentTargetsTests
  ]

nubComponentTargetsTests :: [TestTree]
nubComponentTargetsTests =
  [ testCase "Works on empty list" $
      nubComponentTargets [] @?= ([] :: [(ComponentTarget, NonEmpty Int)])
  , testCase "Merges targets to same component" $
      nubComponentTargets
        [ (mainLibModuleTarget, 1 :: Int)
        , (mainLibFileTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [2])]
  , testCase "Merges whole component targets" $
      nubComponentTargets [(mainLibFileTarget, 2), (mainLibWholeCompTarget, 1 :: Int)]
        @?= [(mainLibWholeCompTarget, 2 :| [1])]
  , testCase "Don't merge unrelated targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (exeWholeCompTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, pure 1), (exeWholeCompTarget, pure 2)]
  , testCase "Merge multiple related targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [4]), (exeWholeCompTarget, 2 :| [3])]
  , testCase "Merge related targets, don't merge unrelated ones" $
      nubComponentTargets
        [ (mainLibFileTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        , (exe2FileTarget, 5)
        ]
        @?= [ (mainLibWholeCompTarget, 1 :| [4])
            , (exeWholeCompTarget, 2 :| [3])
            , (exe2WholeCompTarget, 5 :| [])
            ]
  ]

-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

mainLibWholeCompTarget :: ComponentTarget
mainLibWholeCompTarget = ComponentTarget (CLibName LMainLibName) WholeComponent

mainLibModuleTarget :: ComponentTarget
mainLibModuleTarget = ComponentTarget (CLibName LMainLibName) (ModuleTarget "Lib")

mainLibFileTarget :: ComponentTarget
mainLibFileTarget = ComponentTarget (CLibName LMainLibName) (FileTarget "./Lib.hs")

exeWholeCompTarget :: ComponentTarget
exeWholeCompTarget = ComponentTarget (CExeName "exe") WholeComponent

exeFileTarget :: ComponentTarget
exeFileTarget = ComponentTarget (CExeName "exe") (FileTarget "./Main.hs")

exe2WholeCompTarget :: ComponentTarget
exe2WholeCompTarget = ComponentTarget (CExeName "exe2") WholeComponent

exe2FileTarget :: ComponentTarget
exe2FileTarget = ComponentTarget (CExeName "exe2") (FileTarget "./Main2.hs")
