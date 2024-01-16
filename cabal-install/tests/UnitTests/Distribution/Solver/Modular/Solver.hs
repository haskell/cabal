{-# LANGUAGE OverloadedStrings #-}

-- | This is a set of unit tests for the dependency solver,
-- which uses the solver DSL ("UnitTests.Distribution.Solver.Modular.DSL")
-- to more conveniently create package databases to run the solver tests on.
module UnitTests.Distribution.Solver.Modular.Solver (tests)
where

-- base
import Data.List (isInfixOf)

import qualified Distribution.Version as V

-- test-framework
import Test.Tasty as TF

-- Cabal
import Language.Haskell.Extension
  ( Extension (..)
  , KnownExtension (..)
  , Language (..)
  )

-- cabal-install
import Distribution.Solver.Types.Flag
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackagePath as P
import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils

tests :: [TF.TestTree]
tests =
  [ testGroup
      "Simple dependencies"
      [ runTest $ mkTest db1 "alreadyInstalled" ["A"] (solverSuccess [])
      , runTest $ mkTest db1 "installLatest" ["B"] (solverSuccess [("B", 2)])
      , runTest $
          preferOldest $
            mkTest db1 "installOldest" ["B"] (solverSuccess [("B", 1)])
      , runTest $ mkTest db1 "simpleDep1" ["C"] (solverSuccess [("B", 1), ("C", 1)])
      , runTest $ mkTest db1 "simpleDep2" ["D"] (solverSuccess [("B", 2), ("D", 1)])
      , runTest $ mkTest db1 "failTwoVersions" ["C", "D"] anySolverFailure
      , runTest $ indep $ mkTest db1 "indepTwoVersions" ["C", "D"] (solverSuccess [("B", 1), ("B", 2), ("C", 1), ("D", 1)])
      , runTest $ indep $ mkTest db1 "aliasWhenPossible1" ["C", "E"] (solverSuccess [("B", 1), ("C", 1), ("E", 1)])
      , runTest $ indep $ mkTest db1 "aliasWhenPossible2" ["D", "E"] (solverSuccess [("B", 2), ("D", 1), ("E", 1)])
      , runTest $ indep $ mkTest db2 "aliasWhenPossible3" ["C", "D"] (solverSuccess [("A", 1), ("A", 2), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
      , runTest $ mkTest db1 "buildDepAgainstOld" ["F"] (solverSuccess [("B", 1), ("E", 1), ("F", 1)])
      , runTest $ mkTest db1 "buildDepAgainstNew" ["G"] (solverSuccess [("B", 2), ("E", 1), ("G", 1)])
      , runTest $ indep $ mkTest db1 "multipleInstances" ["F", "G"] anySolverFailure
      , runTest $ mkTest db21 "unknownPackage1" ["A"] (solverSuccess [("A", 1), ("B", 1)])
      , runTest $ mkTest db22 "unknownPackage2" ["A"] (solverFailure (isInfixOf "unknown package: C"))
      , runTest $ mkTest db23 "unknownPackage3" ["A"] (solverFailure (isInfixOf "unknown package: B"))
      , runTest $ mkTest [] "unknown target" ["A"] (solverFailure (isInfixOf "unknown package: A"))
      ]
  , testGroup
      "Flagged dependencies"
      [ runTest $ mkTest db3 "forceFlagOn" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTest db3 "forceFlagOff" ["D"] (solverSuccess [("A", 2), ("B", 1), ("D", 1)])
      , runTest $ indep $ mkTest db3 "linkFlags1" ["C", "D"] anySolverFailure
      , runTest $ indep $ mkTest db4 "linkFlags2" ["C", "D"] anySolverFailure
      , runTest $ indep $ mkTest db18 "linkFlags3" ["A", "B"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2), ("F", 1)])
      ]
  , testGroup
      "Lifting dependencies out of conditionals"
      [ runTest $ commonDependencyLogMessage "common dependency log message"
      , runTest $ twoLevelDeepCommonDependencyLogMessage "two level deep common dependency log message"
      , runTest $ testBackjumpingWithCommonDependency "backjumping with common dependency"
      ]
  , testGroup
      "Manual flags"
      [ runTest $
          mkTest dbManualFlags "Use default value for manual flag" ["pkg"] $
            solverSuccess [("pkg", 1), ("true-dep", 1)]
      , let checkFullLog =
              any $ isInfixOf "rejecting: pkg:-flag (manual flag can only be changed explicitly)"
         in runTest $
              setVerbose $
                constraints [ExVersionConstraint (ScopeAnyQualifier "true-dep") V.noVersion] $
                  mkTest dbManualFlags "Don't toggle manual flag to avoid conflict" ["pkg"] $
                    -- TODO: We should check the summarized log instead of the full log
                    -- for the manual flags error message, but it currently only
                    -- appears in the full log.
                    SolverResult checkFullLog (Left $ const True)
      , let cs = [ExFlagConstraint (ScopeAnyQualifier "pkg") "flag" False]
         in runTest $
              constraints cs $
                mkTest dbManualFlags "Toggle manual flag with flag constraint" ["pkg"] $
                  solverSuccess [("false-dep", 1), ("pkg", 1)]
      ]
  , testGroup
      "Qualified manual flag constraints"
      [ let name = "Top-level flag constraint does not constrain setup dep's flag"
            cs = [ExFlagConstraint (ScopeQualified P.QualToplevel "B") "flag" False]
         in runTest $
              constraints cs $
                mkTest dbSetupDepWithManualFlag name ["A"] $
                  solverSuccess
                    [ ("A", 1)
                    , ("B", 1)
                    , ("B", 2)
                    , ("b-1-false-dep", 1)
                    , ("b-2-true-dep", 1)
                    ]
      , let name = "Solver can toggle setup dep's flag to match top-level constraint"
            cs =
              [ ExFlagConstraint (ScopeQualified P.QualToplevel "B") "flag" False
              , ExVersionConstraint (ScopeAnyQualifier "b-2-true-dep") V.noVersion
              ]
         in runTest $
              constraints cs $
                mkTest dbSetupDepWithManualFlag name ["A"] $
                  solverSuccess
                    [ ("A", 1)
                    , ("B", 1)
                    , ("B", 2)
                    , ("b-1-false-dep", 1)
                    , ("b-2-false-dep", 1)
                    ]
      , let name = "User can constrain flags separately with qualified constraints"
            cs =
              [ ExFlagConstraint (ScopeQualified P.QualToplevel "B") "flag" True
              , ExFlagConstraint (ScopeQualified (P.QualSetup "A") "B") "flag" False
              ]
         in runTest $
              constraints cs $
                mkTest dbSetupDepWithManualFlag name ["A"] $
                  solverSuccess
                    [ ("A", 1)
                    , ("B", 1)
                    , ("B", 2)
                    , ("b-1-true-dep", 1)
                    , ("b-2-false-dep", 1)
                    ]
      , -- Regression test for #4299
        let name = "Solver can link deps when only one has constrained manual flag"
            cs = [ExFlagConstraint (ScopeQualified P.QualToplevel "B") "flag" False]
         in runTest $
              constraints cs $
                mkTest dbLinkedSetupDepWithManualFlag name ["A"] $
                  solverSuccess [("A", 1), ("B", 1), ("b-1-false-dep", 1)]
      , let name = "Solver cannot link deps that have conflicting manual flag constraints"
            cs =
              [ ExFlagConstraint (ScopeQualified P.QualToplevel "B") "flag" True
              , ExFlagConstraint (ScopeQualified (P.QualSetup "A") "B") "flag" False
              ]
            failureReason = "(constraint from unknown source requires opposite flag selection)"
            checkFullLog lns =
              all
                (\msg -> any (msg `isInfixOf`) lns)
                [ "rejecting: B:-flag " ++ failureReason
                , "rejecting: A:setup.B:+flag " ++ failureReason
                ]
         in runTest $
              constraints cs $
                setVerbose $
                  mkTest dbLinkedSetupDepWithManualFlag name ["A"] $
                    SolverResult checkFullLog (Left $ const True)
      ]
  , testGroup
      "Stanzas"
      [ runTest $ enableAllTests $ mkTest db5 "simpleTest1" ["C"] (solverSuccess [("A", 2), ("C", 1)])
      , runTest $ enableAllTests $ mkTest db5 "simpleTest2" ["D"] anySolverFailure
      , runTest $ enableAllTests $ mkTest db5 "simpleTest3" ["E"] (solverSuccess [("A", 1), ("E", 1)])
      , runTest $ enableAllTests $ mkTest db5 "simpleTest4" ["F"] anySolverFailure -- TODO
      , runTest $ enableAllTests $ mkTest db5 "simpleTest5" ["G"] (solverSuccess [("A", 2), ("G", 1)])
      , runTest $ enableAllTests $ mkTest db5 "simpleTest6" ["E", "G"] anySolverFailure
      , runTest $ indep $ enableAllTests $ mkTest db5 "simpleTest7" ["E", "G"] (solverSuccess [("A", 1), ("A", 2), ("E", 1), ("G", 1)])
      , runTest $ enableAllTests $ mkTest db6 "depsWithTests1" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ indep $ enableAllTests $ mkTest db6 "depsWithTests2" ["C", "D"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1)])
      , runTest $ testTestSuiteWithFlag "test suite with flag"
      ]
  , testGroup
      "Setup dependencies"
      [ runTest $ mkTest db7 "setupDeps1" ["B"] (solverSuccess [("A", 2), ("B", 1)])
      , runTest $ mkTest db7 "setupDeps2" ["C"] (solverSuccess [("A", 2), ("C", 1)])
      , runTest $ mkTest db7 "setupDeps3" ["D"] (solverSuccess [("A", 1), ("D", 1)])
      , runTest $ mkTest db7 "setupDeps4" ["E"] (solverSuccess [("A", 1), ("A", 2), ("E", 1)])
      , runTest $ mkTest db7 "setupDeps5" ["F"] (solverSuccess [("A", 1), ("A", 2), ("F", 1)])
      , runTest $ mkTest db8 "setupDeps6" ["C", "D"] (solverSuccess [("A", 1), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
      , runTest $ mkTest db9 "setupDeps7" ["F", "G"] (solverSuccess [("A", 1), ("B", 1), ("B", 2), ("C", 1), ("D", 1), ("E", 1), ("E", 2), ("F", 1), ("G", 1)])
      , runTest $ mkTest db10 "setupDeps8" ["C"] (solverSuccess [("C", 1)])
      , runTest $ indep $ mkTest dbSetupDeps "setupDeps9" ["A", "B"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2)])
      ]
  , testGroup
      "Base shim"
      [ runTest $ mkTest db11 "baseShim1" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTest db12 "baseShim2" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTest db12 "baseShim3" ["B"] (solverSuccess [("B", 1)])
      , runTest $ mkTest db12 "baseShim4" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTest db12 "baseShim5" ["D"] anySolverFailure
      , runTest $ mkTest db12 "baseShim6" ["E"] (solverSuccess [("E", 1), ("syb", 2)])
      ]
  , testGroup
      "Base and non-reinstallable"
      [ runTest $
          mkTest dbBase "Refuse to install base without --allow-boot-library-installs" ["base"] $
            solverFailure (isInfixOf "rejecting: base-1.0.0 (constraint from non-reinstallable package requires installed instance)")
      , runTest $
          allowBootLibInstalls $
            mkTest dbBase "Install base with --allow-boot-library-installs" ["base"] $
              solverSuccess [("base", 1), ("ghc-prim", 1), ("integer-gmp", 1), ("integer-simple", 1)]
      , runTest $
          mkTest dbNonupgrade "Refuse to install newer ghc requested by another library" ["A"] $
            solverFailure (isInfixOf "rejecting: ghc-2.0.0 (constraint from non-reinstallable package requires installed instance)")
      ]
  , testGroup
      "reject-unconstrained"
      [ runTest $
          onlyConstrained $
            mkTest db12 "missing syb" ["E"] $
              solverFailure (isInfixOf "not a user-provided goal")
      , runTest $
          onlyConstrained $
            mkTest db12 "all goals" ["E", "syb"] $
              solverSuccess [("E", 1), ("syb", 2)]
      , runTest $
          onlyConstrained $
            mkTest db17 "backtracking" ["A", "B"] $
              solverSuccess [("A", 2), ("B", 1)]
      , runTest $
          onlyConstrained $
            mkTest db17 "failure message" ["A"] $
              solverFailure $
                isInfixOf $
                  "Could not resolve dependencies:\n"
                    ++ "[__0] trying: A-3.0.0 (user goal)\n"
                    ++ "[__1] next goal: C (dependency of A)\n"
                    ++ "[__1] fail (not a user-provided goal nor mentioned as a constraint, "
                    ++ "but reject-unconstrained-dependencies was set)\n"
                    ++ "[__1] fail (backjumping, conflict set: A, C)\n"
                    ++ "After searching the rest of the dependency tree exhaustively, "
                    ++ "these were the goals I've had most trouble fulfilling: A, C, B"
      ]
  , testGroup
      "Cycles"
      [ runTest $ mkTest db14 "simpleCycle1" ["A"] anySolverFailure
      , runTest $ mkTest db14 "simpleCycle2" ["A", "B"] anySolverFailure
      , runTest $ mkTest db14 "cycleWithFlagChoice1" ["C"] (solverSuccess [("C", 1), ("E", 1)])
      , runTest $ mkTest db15 "cycleThroughSetupDep1" ["A"] anySolverFailure
      , runTest $ mkTest db15 "cycleThroughSetupDep2" ["B"] anySolverFailure
      , runTest $ mkTest db15 "cycleThroughSetupDep3" ["C"] (solverSuccess [("C", 2), ("D", 1)])
      , runTest $ mkTest db15 "cycleThroughSetupDep4" ["D"] (solverSuccess [("D", 1)])
      , runTest $ mkTest db15 "cycleThroughSetupDep5" ["E"] (solverSuccess [("C", 2), ("D", 1), ("E", 1)])
      , runTest $ issue4161 "detect cycle between package and its setup script"
      , runTest $ testCyclicDependencyErrorMessages "cyclic dependency error messages"
      ]
  , testGroup
      "Extensions"
      [ runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupported" ["A"] anySolverFailure
      , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupportedIndirect" ["B"] anySolverFailure
      , runTest $ mkTestExts [EnableExtension RankNTypes] dbExts1 "supported" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTestExts (map EnableExtension [CPP, RankNTypes]) dbExts1 "supportedIndirect" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "disabledExtension" ["D"] anySolverFailure
      , runTest $ mkTestExts (map EnableExtension [CPP, RankNTypes]) dbExts1 "disabledExtension" ["D"] anySolverFailure
      , runTest $ mkTestExts (UnknownExtension "custom" : map EnableExtension [CPP, RankNTypes]) dbExts1 "supportedUnknown" ["E"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("E", 1)])
      ]
  , testGroup
      "Languages"
      [ runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupported" ["A"] anySolverFailure
      , runTest $ mkTestLangs [Haskell98, Haskell2010] dbLangs1 "supported" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupportedIndirect" ["B"] anySolverFailure
      , runTest $ mkTestLangs [Haskell98, Haskell2010, UnknownLanguage "Haskell3000"] dbLangs1 "supportedUnknown" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      ]
  , testGroup
      "Qualified Package Constraints"
      [ runTest $
          mkTest dbConstraints "install latest versions without constraints" ["A", "B", "C"] $
            solverSuccess [("A", 7), ("B", 8), ("C", 9), ("D", 7), ("D", 8), ("D", 9)]
      , let cs = [ExVersionConstraint (ScopeAnyQualifier "D") $ mkVersionRange 1 4]
         in runTest $
              constraints cs $
                mkTest dbConstraints "force older versions with unqualified constraint" ["A", "B", "C"] $
                  solverSuccess [("A", 1), ("B", 2), ("C", 3), ("D", 1), ("D", 2), ("D", 3)]
      , let cs =
              [ ExVersionConstraint (ScopeQualified P.QualToplevel "D") $ mkVersionRange 1 4
              , ExVersionConstraint (ScopeQualified (P.QualSetup "B") "D") $ mkVersionRange 4 7
              ]
         in runTest $
              constraints cs $
                mkTest dbConstraints "force multiple versions with qualified constraints" ["A", "B", "C"] $
                  solverSuccess [("A", 1), ("B", 5), ("C", 9), ("D", 1), ("D", 5), ("D", 9)]
      , let cs = [ExVersionConstraint (ScopeAnySetupQualifier "D") $ mkVersionRange 1 4]
         in runTest $
              constraints cs $
                mkTest dbConstraints "constrain package across setup scripts" ["A", "B", "C"] $
                  solverSuccess [("A", 7), ("B", 2), ("C", 3), ("D", 2), ("D", 3), ("D", 7)]
      ]
  , testGroup
      "Package Preferences"
      [ runTest $ preferences [ExPkgPref "A" $ mkvrThis 1] $ mkTest db13 "selectPreferredVersionSimple" ["A"] (solverSuccess [("A", 1)])
      , runTest $ preferences [ExPkgPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionSimple2" ["A"] (solverSuccess [("A", 2)])
      , runTest
          $ preferences
            [ ExPkgPref "A" $ mkvrOrEarlier 2
            , ExPkgPref "A" $ mkvrOrEarlier 1
            ]
          $ mkTest db13 "selectPreferredVersionMultiple" ["A"] (solverSuccess [("A", 1)])
      , runTest
          $ preferences
            [ ExPkgPref "A" $ mkvrOrEarlier 1
            , ExPkgPref "A" $ mkvrOrEarlier 2
            ]
          $ mkTest db13 "selectPreferredVersionMultiple2" ["A"] (solverSuccess [("A", 1)])
      , runTest
          $ preferences
            [ ExPkgPref "A" $ mkvrThis 1
            , ExPkgPref "A" $ mkvrThis 2
            ]
          $ mkTest db13 "selectPreferredVersionMultiple3" ["A"] (solverSuccess [("A", 2)])
      , runTest
          $ preferences
            [ ExPkgPref "A" $ mkvrThis 1
            , ExPkgPref "A" $ mkvrOrEarlier 2
            ]
          $ mkTest db13 "selectPreferredVersionMultiple4" ["A"] (solverSuccess [("A", 1)])
      ]
  , testGroup
      "Stanza Preferences"
      [ runTest $
          mkTest dbStanzaPreferences1 "disable tests by default" ["pkg"] $
            solverSuccess [("pkg", 1)]
      , runTest $
          preferences [ExStanzaPref "pkg" [TestStanzas]] $
            mkTest dbStanzaPreferences1 "enable tests with testing preference" ["pkg"] $
              solverSuccess [("pkg", 1), ("test-dep", 1)]
      , runTest $
          preferences [ExStanzaPref "pkg" [TestStanzas]] $
            mkTest dbStanzaPreferences2 "disable testing when it's not possible" ["pkg"] $
              solverSuccess [("pkg", 1)]
      , testStanzaPreference "test stanza preference"
      ]
  , testGroup
      "Buildable Field"
      [ testBuildable "avoid building component with unknown dependency" (ExAny "unknown")
      , testBuildable "avoid building component with unknown extension" (ExExt (UnknownExtension "unknown"))
      , testBuildable "avoid building component with unknown language" (ExLang (UnknownLanguage "unknown"))
      , runTest $ mkTest dbBuildable1 "choose flags that set buildable to false" ["pkg"] (solverSuccess [("flag1-false", 1), ("flag2-true", 1), ("pkg", 1)])
      , runTest $ mkTest dbBuildable2 "choose version that sets buildable to false" ["A"] (solverSuccess [("A", 1), ("B", 2)])
      ]
  , testGroup
      "Pkg-config dependencies"
      [ runTest $ mkTestPCDepends (Just []) dbPC1 "noPkgs" ["A"] anySolverFailure
      , runTest $ mkTestPCDepends (Just [("pkgA", "0")]) dbPC1 "tooOld" ["A"] anySolverFailure
      , runTest $ mkTestPCDepends (Just [("pkgA", "1.0.0"), ("pkgB", "1.0.0")]) dbPC1 "pruneNotFound" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTestPCDepends (Just [("pkgA", "1.0.0"), ("pkgB", "2.0.0")]) dbPC1 "chooseNewest" ["C"] (solverSuccess [("A", 1), ("B", 2), ("C", 1)])
      , runTest $ mkTestPCDepends Nothing dbPC1 "noPkgConfigFailure" ["A"] anySolverFailure
      , runTest $ mkTestPCDepends Nothing dbPC1 "noPkgConfigSuccess" ["D"] (solverSuccess [("D", 1)])
      ]
  , testGroup
      "Independent goals"
      [ runTest $ indep $ mkTest db16 "indepGoals1" ["A", "B"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2), ("E", 1)])
      , runTest $ testIndepGoals2 "indepGoals2"
      , runTest $ testIndepGoals3 "indepGoals3"
      , runTest $ testIndepGoals4 "indepGoals4"
      , runTest $ testIndepGoals5 "indepGoals5 - fixed goal order" FixedGoalOrder
      , runTest $ testIndepGoals5 "indepGoals5 - default goal order" DefaultGoalOrder
      , runTest $ testIndepGoals6 "indepGoals6 - fixed goal order" FixedGoalOrder
      , runTest $ testIndepGoals6 "indepGoals6 - default goal order" DefaultGoalOrder
      ]
  , -- Tests designed for the backjumping blog post
    testGroup
      "Backjumping"
      [ runTest $ mkTest dbBJ1a "bj1a" ["A"] (solverSuccess [("A", 1), ("B", 1)])
      , runTest $ mkTest dbBJ1b "bj1b" ["A"] (solverSuccess [("A", 1), ("B", 1)])
      , runTest $ mkTest dbBJ1c "bj1c" ["A"] (solverSuccess [("A", 1), ("B", 1)])
      , runTest $ mkTest dbBJ2 "bj2" ["A"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTest dbBJ3 "bj3" ["A"] (solverSuccess [("A", 1), ("Ba", 1), ("C", 1)])
      , runTest $ mkTest dbBJ4 "bj4" ["A"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ mkTest dbBJ5 "bj5" ["A"] (solverSuccess [("A", 1), ("B", 1), ("D", 1)])
      , runTest $ mkTest dbBJ6 "bj6" ["A"] (solverSuccess [("A", 1), ("B", 1)])
      , runTest $ mkTest dbBJ7 "bj7" ["A"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      , runTest $ indep $ mkTest dbBJ8 "bj8" ["A", "B"] (solverSuccess [("A", 1), ("B", 1), ("C", 1)])
      ]
  , testGroup
      "main library dependencies"
      [ let db = [Right $ exAvNoLibrary "A" 1 `withExe` exExe "exe" []]
         in runTest $
              mkTest db "install build target without a library" ["A"] $
                solverSuccess [("A", 1)]
      , let db =
              [ Right $ exAv "A" 1 [ExAny "B"]
              , Right $ exAvNoLibrary "B" 1 `withExe` exExe "exe" []
              ]
         in runTest $
              mkTest db "reject build-depends dependency with no library" ["A"] $
                solverFailure (isInfixOf "rejecting: B-1.0.0 (does not contain library, which is required by A)")
      , let exe = exExe "exe" []
            db =
              [ Right $ exAv "A" 1 [ExAny "B"]
              , Right $ exAvNoLibrary "B" 2 `withExe` exe
              , Right $ exAv "B" 1 [] `withExe` exe
              ]
         in runTest $
              mkTest db "choose version of build-depends dependency that has a library" ["A"] $
                solverSuccess [("A", 1), ("B", 1)]
      ]
  , testGroup
      "sub-library dependencies"
      [ let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib"]
              , Right $ exAv "B" 1 []
              ]
         in runTest $
              mkTest db "reject package that is missing required sub-library" ["A"] $
                solverFailure $
                  isInfixOf $
                    "rejecting: B-1.0.0 (does not contain library 'sub-lib', which is required by A)"
      , let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib"]
              , Right $ exAvNoLibrary "B" 1 `withSubLibrary` exSubLib "sub-lib" []
              ]
         in runTest $
              mkTest db "reject package with private but required sub-library" ["A"] $
                solverFailure $
                  isInfixOf $
                    "rejecting: B-1.0.0 (library 'sub-lib' is private, but it is required by A)"
      , let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib"]
              , Right $
                  exAvNoLibrary "B" 1
                    `withSubLibrary` exSubLib "sub-lib" [ExFlagged "make-lib-private" (dependencies []) publicDependencies]
              ]
         in runTest $
              constraints [ExFlagConstraint (ScopeAnyQualifier "B") "make-lib-private" True] $
                mkTest db "reject package with sub-library made private by flag constraint" ["A"] $
                  solverFailure $
                    isInfixOf $
                      "rejecting: B-1.0.0 (library 'sub-lib' is private, but it is required by A)"
      , let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib"]
              , Right $
                  exAvNoLibrary "B" 1
                    `withSubLibrary` exSubLib "sub-lib" [ExFlagged "make-lib-private" (dependencies []) publicDependencies]
              ]
         in runTest $
              mkTest db "treat sub-library as visible even though flag choice could make it private" ["A"] $
                solverSuccess [("A", 1), ("B", 1)]
      , let db =
              [ Right $ exAv "A" 1 [ExAny "B"]
              , Right $ exAv "B" 1 [] `withSubLibrary` exSubLib "sub-lib" []
              , Right $ exAv "C" 1 [ExSubLibAny "B" "sub-lib"]
              ]
            goals :: [ExampleVar]
            goals =
              [ P QualNone "A"
              , P QualNone "B"
              , P QualNone "C"
              ]
         in runTest $
              goalOrder goals $
                mkTest db "reject package that requires a private sub-library" ["A", "C"] $
                  solverFailure $
                    isInfixOf $
                      "rejecting: C-1.0.0 (requires library 'sub-lib' from B, but the component is private)"
      , let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib-v1"]
              , Right $ exAv "B" 2 [] `withSubLibrary` ExSubLib "sub-lib-v2" publicDependencies
              , Right $ exAv "B" 1 [] `withSubLibrary` ExSubLib "sub-lib-v1" publicDependencies
              ]
         in runTest $
              mkTest db "choose version of package containing correct sub-library" ["A"] $
                solverSuccess [("A", 1), ("B", 1)]
      , let db =
              [ Right $ exAv "A" 1 [ExSubLibAny "B" "sub-lib"]
              , Right $ exAv "B" 2 [] `withSubLibrary` ExSubLib "sub-lib" (dependencies [])
              , Right $ exAv "B" 1 [] `withSubLibrary` ExSubLib "sub-lib" publicDependencies
              ]
         in runTest $
              mkTest db "choose version of package with public sub-library" ["A"] $
                solverSuccess [("A", 1), ("B", 1)]
      ]
  , -- build-tool-depends dependencies
    testGroup
      "build-tool-depends"
      [ runTest $ mkTest dbBuildTools "simple exe dependency" ["A"] (solverSuccess [("A", 1), ("bt-pkg", 2)])
      , runTest $
          disableSolveExecutables $
            mkTest dbBuildTools "don't install build tool packages in legacy mode" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTest dbBuildTools "flagged exe dependency" ["B"] (solverSuccess [("B", 1), ("bt-pkg", 2)])
      , runTest $
          enableAllTests $
            mkTest dbBuildTools "test suite exe dependency" ["C"] (solverSuccess [("C", 1), ("bt-pkg", 2)])
      , runTest $
          mkTest dbBuildTools "unknown exe" ["D"] $
            solverFailure (isInfixOf "does not contain executable 'unknown-exe', which is required by D")
      , runTest $
          disableSolveExecutables $
            mkTest dbBuildTools "don't check for build tool executables in legacy mode" ["D"] $
              solverSuccess [("D", 1)]
      , runTest $
          mkTest dbBuildTools "unknown build tools package error mentions package, not exe" ["E"] $
            solverFailure (isInfixOf "unknown package: E:unknown-pkg:exe.unknown-pkg (dependency of E)")
      , runTest $
          mkTest dbBuildTools "unknown flagged exe" ["F"] $
            solverFailure (isInfixOf "does not contain executable 'unknown-exe', which is required by F +flagF")
      , runTest $
          enableAllTests $
            mkTest dbBuildTools "unknown test suite exe" ["G"] $
              solverFailure (isInfixOf "does not contain executable 'unknown-exe', which is required by G *test")
      , runTest $
          mkTest dbBuildTools "wrong exe for build tool package version" ["H"] $
            solverFailure $
              isInfixOf $
                -- The solver reports the version conflict when a version conflict
                -- and an executable conflict apply to the same package version.
                "[__1] rejecting: H:bt-pkg:exe.bt-pkg-4.0.0 (conflict: H => H:bt-pkg:exe.bt-pkg (exe exe1)==3.0.0)\n"
                  ++ "[__1] rejecting: H:bt-pkg:exe.bt-pkg-3.0.0 (does not contain executable 'exe1', which is required by H)\n"
                  ++ "[__1] rejecting: H:bt-pkg:exe.bt-pkg-2.0.0 (conflict: H => H:bt-pkg:exe.bt-pkg (exe exe1)==3.0.0)"
      , runTest $ chooseExeAfterBuildToolsPackage True "choose exe after choosing its package - success"
      , runTest $ chooseExeAfterBuildToolsPackage False "choose exe after choosing its package - failure"
      , runTest $ rejectInstalledBuildToolPackage "reject installed package for build-tool dependency"
      , runTest $ requireConsistentBuildToolVersions "build tool versions must be consistent within one package"
      ]
  , -- build-tools dependencies
    testGroup
      "legacy build-tools"
      [ runTest $ mkTest dbLegacyBuildTools1 "bt1" ["A"] (solverSuccess [("A", 1), ("alex", 1)])
      , runTest $
          disableSolveExecutables $
            mkTest dbLegacyBuildTools1 "bt1 - don't install build tool packages in legacy mode" ["A"] (solverSuccess [("A", 1)])
      , runTest $
          mkTest dbLegacyBuildTools2 "bt2" ["A"] $
            solverFailure (isInfixOf "does not contain executable 'alex', which is required by A")
      , runTest $
          disableSolveExecutables $
            mkTest dbLegacyBuildTools2 "bt2 - don't check for build tool executables in legacy mode" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTest dbLegacyBuildTools3 "bt3" ["A"] (solverSuccess [("A", 1)])
      , runTest $ mkTest dbLegacyBuildTools4 "bt4" ["C"] (solverSuccess [("A", 1), ("B", 1), ("C", 1), ("alex", 1), ("alex", 2)])
      , runTest $ mkTest dbLegacyBuildTools5 "bt5" ["B"] (solverSuccess [("A", 1), ("A", 2), ("B", 1), ("alex", 1)])
      , runTest $ mkTest dbLegacyBuildTools6 "bt6" ["A"] (solverSuccess [("A", 1), ("alex", 1), ("happy", 1)])
      ]
  , -- internal dependencies
    testGroup
      "internal dependencies"
      [ runTest $ mkTest dbIssue3775 "issue #3775" ["B"] (solverSuccess [("A", 2), ("B", 2), ("warp", 1)])
      ]
  , -- tests for partial fix for issue #5325
    testGroup "Components that are unbuildable in the current environment" $
      let flagConstraint = ExFlagConstraint . ScopeAnyQualifier
       in [ let db = [Right $ exAv "A" 1 [ExFlagged "build-lib" (dependencies []) unbuildableDependencies]]
             in runTest $
                  constraints [flagConstraint "A" "build-lib" False] $
                    mkTest db "install unbuildable library" ["A"] $
                      solverSuccess [("A", 1)]
          , let db =
                  [ Right $
                      exAvNoLibrary "A" 1
                        `withExe` exExe "exe" [ExFlagged "build-exe" (dependencies []) unbuildableDependencies]
                  ]
             in runTest $
                  constraints [flagConstraint "A" "build-exe" False] $
                    mkTest db "install unbuildable exe" ["A"] $
                      solverSuccess [("A", 1)]
          , let db =
                  [ Right $ exAv "A" 1 [ExAny "B"]
                  , Right $ exAv "B" 1 [ExFlagged "build-lib" (dependencies []) unbuildableDependencies]
                  ]
             in runTest $
                  constraints [flagConstraint "B" "build-lib" False] $
                    mkTest db "reject library dependency with unbuildable library" ["A"] $
                      solverFailure $
                        isInfixOf $
                          "rejecting: B-1.0.0 (library is not buildable in the "
                            ++ "current environment, but it is required by A)"
          , let db =
                  [ Right $ exAv "A" 1 [ExBuildToolAny "B" "bt"]
                  , Right $
                      exAv "B" 1 [ExFlagged "build-lib" (dependencies []) unbuildableDependencies]
                        `withExe` exExe "bt" []
                  ]
             in runTest $
                  constraints [flagConstraint "B" "build-lib" False] $
                    mkTest db "allow build-tool dependency with unbuildable library" ["A"] $
                      solverSuccess [("A", 1), ("B", 1)]
          , let db =
                  [ Right $ exAv "A" 1 [ExBuildToolAny "B" "bt"]
                  , Right $
                      exAv "B" 1 []
                        `withExe` exExe "bt" [ExFlagged "build-exe" (dependencies []) unbuildableDependencies]
                  ]
             in runTest $
                  constraints [flagConstraint "B" "build-exe" False] $
                    mkTest db "reject build-tool dependency with unbuildable exe" ["A"] $
                      solverFailure $
                        isInfixOf $
                          "rejecting: A:B:exe.B-1.0.0 (executable 'bt' is not "
                            ++ "buildable in the current environment, but it is required by A)"
          , runTest $
              chooseUnbuildableExeAfterBuildToolsPackage
                "choose unbuildable exe after choosing its package"
          ]
  , testGroup
      "--fine-grained-conflicts"
      [ -- Skipping a version because of a problematic dependency:
        --
        -- When the solver explores A-4, it finds that it cannot satisfy B's
        -- dependencies. This allows the solver to skip the subsequent
        -- versions of A that also depend on B.
        runTest $
          let db =
                [ Right $ exAv "A" 4 [ExAny "B"]
                , Right $ exAv "A" 3 [ExAny "B"]
                , Right $ exAv "A" 2 [ExAny "B"]
                , Right $ exAv "A" 1 []
                , Right $ exAv "B" 2 [ExAny "unknown1"]
                , Right $ exAv "B" 1 [ExAny "unknown2"]
                ]
              msg =
                [ "[__0] trying: A-4.0.0 (user goal)"
                , "[__1] trying: B-2.0.0 (dependency of A)"
                , "[__2] unknown package: unknown1 (dependency of B)"
                , "[__2] fail (backjumping, conflict set: B, unknown1)"
                , "[__1] trying: B-1.0.0"
                , "[__2] unknown package: unknown2 (dependency of B)"
                , "[__2] fail (backjumping, conflict set: B, unknown2)"
                , "[__1] fail (backjumping, conflict set: A, B, unknown1, unknown2)"
                , "[__0] skipping: A; 3.0.0, 2.0.0 (has the same characteristics that "
                    ++ "caused the previous version to fail: depends on 'B')"
                , "[__0] trying: A-1.0.0"
                , "[__1] done"
                ]
           in setVerbose $
                mkTest db "skip version due to problematic dependency" ["A"] $
                  SolverResult (isInfixOf msg) $
                    Right [("A", 1)]
      , -- Skipping a version because of a restrictive constraint on a
        -- dependency:
        --
        -- The solver rejects A-4 because its constraint on B excludes B-1.
        -- Then the solver is able to skip A-3 and A-2 because they also
        -- exclude B-1, even though they don't have the exact same constraints
        -- on B.
        runTest $
          let db =
                [ Right $ exAv "A" 4 [ExFix "B" 14]
                , Right $ exAv "A" 3 [ExFix "B" 13]
                , Right $ exAv "A" 2 [ExFix "B" 12]
                , Right $ exAv "A" 1 [ExFix "B" 11]
                , Right $ exAv "B" 11 []
                ]
              msg =
                [ "[__0] trying: A-4.0.0 (user goal)"
                , "[__1] next goal: B (dependency of A)"
                , "[__1] rejecting: B-11.0.0 (conflict: A => B==14.0.0)"
                , "[__1] fail (backjumping, conflict set: A, B)"
                , "[__0] skipping: A; 3.0.0, 2.0.0 (has the same characteristics that "
                    ++ "caused the previous version to fail: depends on 'B' but excludes "
                    ++ "version 11.0.0)"
                , "[__0] trying: A-1.0.0"
                , "[__1] next goal: B (dependency of A)"
                , "[__1] trying: B-11.0.0"
                , "[__2] done"
                ]
           in setVerbose $
                mkTest db "skip version due to restrictive constraint on its dependency" ["A"] $
                  SolverResult (isInfixOf msg) $
                    Right [("A", 1), ("B", 11)]
      , -- This test tests the case where the solver chooses a version for one
        -- package, B, before choosing a version for one of its reverse
        -- dependencies, C. While the solver is exploring the subtree rooted
        -- at B-3, it finds that C-2's dependency on B conflicts with B-3.
        -- Then the solver is able to skip C-1, because it also excludes B-3.
        --
        -- --fine-grained-conflicts could have a benefit in this case even
        -- though the solver would have found the conflict between B-3 and C-1
        -- immediately after trying C-1 anyway. It prevents C-1 from
        -- introducing any other conflicts which could increase the size of
        -- the conflict set.
        runTest $
          let db =
                [ Right $ exAv "A" 1 [ExAny "B", ExAny "C"]
                , Right $ exAv "B" 3 []
                , Right $ exAv "B" 2 []
                , Right $ exAv "B" 1 []
                , Right $ exAv "C" 2 [ExFix "B" 2]
                , Right $ exAv "C" 1 [ExFix "B" 1]
                ]
              goals = [P QualNone pkg | pkg <- ["A", "B", "C"]]
              expectedMsg =
                [ "[__0] trying: A-1.0.0 (user goal)"
                , "[__1] trying: B-3.0.0 (dependency of A)"
                , "[__2] next goal: C (dependency of A)"
                , "[__2] rejecting: C-2.0.0 (conflict: B==3.0.0, C => B==2.0.0)"
                , "[__2] skipping: C-1.0.0 (has the same characteristics that caused the "
                    ++ "previous version to fail: excludes 'B' version 3.0.0)"
                , "[__2] fail (backjumping, conflict set: A, B, C)"
                , "[__1] trying: B-2.0.0"
                , "[__2] next goal: C (dependency of A)"
                , "[__2] trying: C-2.0.0"
                , "[__3] done"
                ]
           in setVerbose $
                goalOrder goals $
                  mkTest db "skip version that excludes dependency that was already chosen" ["A"] $
                    SolverResult (isInfixOf expectedMsg) $
                      Right [("A", 1), ("B", 2), ("C", 2)]
      , -- This test tests how the solver merges conflicts when it has
        -- multiple reasons to add a variable to the conflict set. In this
        -- case, package A conflicts with B and C. The solver should take the
        -- union of the conflicts and then only skip a version if it does not
        -- resolve any of the conflicts.
        --
        -- The solver rejects A-3 because it can't find consistent versions for
        -- its two dependencies, B and C. Then it skips A-2 because A-2 also
        -- depends on B and C. This test ensures that the solver considers
        -- A-1 even though A-1 only resolves one of the conflicts (A-1 removes
        -- the dependency on C).
        runTest $
          let db =
                [ Right $ exAv "A" 3 [ExAny "B", ExAny "C"]
                , Right $ exAv "A" 2 [ExAny "B", ExAny "C"]
                , Right $ exAv "A" 1 [ExAny "B"]
                , Right $ exAv "B" 1 [ExFix "D" 1]
                , Right $ exAv "C" 1 [ExFix "D" 2]
                , Right $ exAv "D" 1 []
                , Right $ exAv "D" 2 []
                ]
              goals = [P QualNone pkg | pkg <- ["A", "B", "C", "D"]]
              msg =
                [ "[__0] trying: A-3.0.0 (user goal)"
                , "[__1] trying: B-1.0.0 (dependency of A)"
                , "[__2] trying: C-1.0.0 (dependency of A)"
                , "[__3] next goal: D (dependency of B)"
                , "[__3] rejecting: D-2.0.0 (conflict: B => D==1.0.0)"
                , "[__3] rejecting: D-1.0.0 (conflict: C => D==2.0.0)"
                , "[__3] fail (backjumping, conflict set: B, C, D)"
                , "[__2] fail (backjumping, conflict set: A, B, C, D)"
                , "[__1] fail (backjumping, conflict set: A, B, C, D)"
                , "[__0] skipping: A-2.0.0 (has the same characteristics that caused the "
                    ++ "previous version to fail: depends on 'B'; depends on 'C')"
                , "[__0] trying: A-1.0.0"
                , "[__1] trying: B-1.0.0 (dependency of A)"
                , "[__2] next goal: D (dependency of B)"
                , "[__2] rejecting: D-2.0.0 (conflict: B => D==1.0.0)"
                , "[__2] trying: D-1.0.0"
                , "[__3] done"
                ]
           in setVerbose $
                goalOrder goals $
                  mkTest db "only skip a version if it resolves none of the previous conflicts" ["A"] $
                    SolverResult (isInfixOf msg) $
                      Right [("A", 1), ("B", 1), ("D", 1)]
      , -- This test ensures that the solver log doesn't show all conflicts
        -- that the solver encountered in a subtree. The solver should only
        -- show the conflicts that are contained in the current conflict set.
        --
        -- The goal order forces the solver to try A-4, encounter a conflict
        -- with B-2, try B-1, and then try C. A-4 conflicts with the only
        -- version of C, so the solver backjumps with a conflict set of
        -- {A, C}. When the solver skips the next version of A, the log should
        -- mention the conflict with C but not B.
        runTest $
          let db =
                [ Right $ exAv "A" 4 [ExFix "B" 1, ExFix "C" 1]
                , Right $ exAv "A" 3 [ExFix "B" 1, ExFix "C" 1]
                , Right $ exAv "A" 2 [ExFix "C" 1]
                , Right $ exAv "A" 1 [ExFix "C" 2]
                , Right $ exAv "B" 2 []
                , Right $ exAv "B" 1 []
                , Right $ exAv "C" 2 []
                ]
              goals = [P QualNone pkg | pkg <- ["A", "B", "C"]]
              msg =
                [ "[__0] trying: A-4.0.0 (user goal)"
                , "[__1] next goal: B (dependency of A)"
                , "[__1] rejecting: B-2.0.0 (conflict: A => B==1.0.0)"
                , "[__1] trying: B-1.0.0"
                , "[__2] next goal: C (dependency of A)"
                , "[__2] rejecting: C-2.0.0 (conflict: A => C==1.0.0)"
                , "[__2] fail (backjumping, conflict set: A, C)"
                , "[__0] skipping: A; 3.0.0, 2.0.0 (has the same characteristics that caused the "
                    ++ "previous version to fail: depends on 'C' but excludes version 2.0.0)"
                , "[__0] trying: A-1.0.0"
                , "[__1] next goal: C (dependency of A)"
                , "[__1] trying: C-2.0.0"
                , "[__2] done"
                ]
           in setVerbose $
                goalOrder goals $
                  mkTest db "don't show conflicts that aren't part of the conflict set" ["A"] $
                    SolverResult (isInfixOf msg) $
                      Right [("A", 1), ("C", 2)]
      , -- Tests that the conflict set is properly updated when a version is
        -- skipped due to being excluded by one of its reverse dependencies'
        -- constraints.
        runTest $
          let db =
                [ Right $ exAv "A" 2 [ExFix "B" 3]
                , Right $ exAv "A" 1 [ExFix "B" 1]
                , Right $ exAv "B" 2 []
                , Right $ exAv "B" 1 []
                ]
              msg =
                [ "[__0] trying: A-2.0.0 (user goal)"
                , "[__1] next goal: B (dependency of A)"
                , -- During this step, the solver adds A and B to the
                  -- conflict set, with the details of each package's
                  -- conflict:
                  --
                  -- A: A's constraint rejected B-2.
                  -- B: B was rejected by A's B==3 constraint
                  "[__1] rejecting: B-2.0.0 (conflict: A => B==3.0.0)"
                , -- When the solver skips B-1, it cannot simply reuse the
                  -- previous conflict set. It also needs to update A's
                  -- entry to say that A also rejected B-1. Otherwise, the
                  -- solver wouldn't know that A-1 could resolve one of
                  -- the conflicts encountered while exploring A-2. The
                  -- solver would skip A-1, even though it leads to the
                  -- solution.
                  "[__1] skipping: B-1.0.0 (has the same characteristics that caused "
                    ++ "the previous version to fail: excluded by constraint '==3.0.0' from 'A')"
                , "[__1] fail (backjumping, conflict set: A, B)"
                , "[__0] trying: A-1.0.0"
                , "[__1] next goal: B (dependency of A)"
                , "[__1] rejecting: B-2.0.0 (conflict: A => B==1.0.0)"
                , "[__1] trying: B-1.0.0"
                , "[__2] done"
                ]
           in setVerbose $
                mkTest db "update conflict set after skipping version - 1" ["A"] $
                  SolverResult (isInfixOf msg) $
                    Right [("A", 1), ("B", 1)]
      , -- Tests that the conflict set is properly updated when a version is
        -- skipped due to excluding a version of one of its dependencies.
        -- This test is similar the previous one, with the goal order reversed.
        runTest $
          let db =
                [ Right $ exAv "A" 2 []
                , Right $ exAv "A" 1 []
                , Right $ exAv "B" 2 [ExFix "A" 3]
                , Right $ exAv "B" 1 [ExFix "A" 1]
                ]
              goals = [P QualNone pkg | pkg <- ["A", "B"]]
              msg =
                [ "[__0] trying: A-2.0.0 (user goal)"
                , "[__1] next goal: B (user goal)"
                , "[__1] rejecting: B-2.0.0 (conflict: A==2.0.0, B => A==3.0.0)"
                , "[__1] skipping: B-1.0.0 (has the same characteristics that caused "
                    ++ "the previous version to fail: excludes 'A' version 2.0.0)"
                , "[__1] fail (backjumping, conflict set: A, B)"
                , "[__0] trying: A-1.0.0"
                , "[__1] next goal: B (user goal)"
                , "[__1] rejecting: B-2.0.0 (conflict: A==1.0.0, B => A==3.0.0)"
                , "[__1] trying: B-1.0.0"
                , "[__2] done"
                ]
           in setVerbose $
                goalOrder goals $
                  mkTest db "update conflict set after skipping version - 2" ["A", "B"] $
                    SolverResult (isInfixOf msg) $
                      Right [("A", 1), ("B", 1)]
      ]
  , -- Tests for the contents of the solver's log
    testGroup
      "Solver log"
      [ -- See issue #3203. The solver should only choose a version for A once.
        runTest $
          let db = [Right $ exAv "A" 1 []]

              p :: [String] -> Bool
              p lg =
                elem "targets: A" lg
                  && length (filter ("trying: A" `isInfixOf`) lg) == 1
           in setVerbose $
                mkTest db "deduplicate targets" ["A", "A"] $
                  SolverResult p $
                    Right [("A", 1)]
      , runTest $
          let db = [Right $ exAv "A" 1 [ExAny "B"]]
              msg =
                "After searching the rest of the dependency tree exhaustively, "
                  ++ "these were the goals I've had most trouble fulfilling: A, B"
           in mkTest db "exhaustive search failure message" ["A"] $
                solverFailure (isInfixOf msg)
      , testSummarizedLog "show conflicts from final conflict set after exhaustive search" Nothing $
          "Could not resolve dependencies:\n"
            ++ "[__0] trying: A-1.0.0 (user goal)\n"
            ++ "[__1] unknown package: F (dependency of A)\n"
            ++ "[__1] fail (backjumping, conflict set: A, F)\n"
            ++ "After searching the rest of the dependency tree exhaustively, "
            ++ "these were the goals I've had most trouble fulfilling: A, F"
      , testSummarizedLog "show first conflicts after inexhaustive search" (Just 3) $
          "Could not resolve dependencies:\n"
            ++ "[__0] trying: A-1.0.0 (user goal)\n"
            ++ "[__1] trying: B-3.0.0 (dependency of A)\n"
            ++ "[__2] unknown package: C (dependency of B)\n"
            ++ "[__2] fail (backjumping, conflict set: B, C)\n"
            ++ "Backjump limit reached (currently 3, change with --max-backjumps "
            ++ "or try to run with --reorder-goals).\n"
      , testSummarizedLog "don't show summarized log when backjump limit is too low" (Just 1) $
          "Backjump limit reached (currently 1, change with --max-backjumps "
            ++ "or try to run with --reorder-goals).\n"
            ++ "Failed to generate a summarized dependency solver log due to low backjump limit."
      , testMinimizeConflictSet
          "minimize conflict set with --minimize-conflict-set"
      , testNoMinimizeConflictSet
          "show original conflict set with --no-minimize-conflict-set"
      , runTest $
          let db =
                [ Right $ exAv "my-package" 1 [ExFix "other-package" 3]
                , Left $ exInst "other-package" 2 "other-package-2.0.0" []
                ]
              msg = "rejecting: other-package-2.0.0/installed-2.0.0"
           in mkTest db "show full installed package version (issue #5892)" ["my-package"] $
                solverFailure (isInfixOf msg)
      , runTest $
          let db =
                [ Right $ exAv "my-package" 1 [ExFix "other-package" 3]
                , Left $ exInst "other-package" 2 "other-package-AbCdEfGhIj0123456789" []
                ]
              msg = "rejecting: other-package-2.0.0/installed-AbCdEfGhIj0123456789"
           in mkTest db "show full installed package ABI hash (issue #5892)" ["my-package"] $
                solverFailure (isInfixOf msg)
      , testGroup
          "package versions abbreviation (issue #9559.)"
          [ runTest $
              let db =
                    [ Right $ exAv "A" 1 []
                    , Right $ exAv "A" 2 []
                    , Right $ exAv "B" 1 [ExFix "A" 3]
                    ]
                  rejecting = "rejecting: A-2.0.0"
                  skipping = "skipping: A-1.0.0"
               in mkTest db "show skipping singleton" ["B"] $
                    solverFailure (\msg -> rejecting `isInfixOf` msg && skipping `isInfixOf` msg)
          , runTest $
              let db =
                    [ Left $ exInst "A" 1 "A-1.0.0" []
                    , Left $ exInst "A" 2 "A-2.0.0" []
                    , Right $ exAv "B" 1 [ExFix "A" 3]
                    ]
                  rejecting = "rejecting: A-2.0.0/installed-2.0.0"
                  skipping = "skipping: A-1.0.0/installed-1.0.0"
               in mkTest db "show skipping singleton, installed" ["B"] $
                    solverFailure (\msg -> rejecting `isInfixOf` msg && skipping `isInfixOf` msg)
          , runTest $
              let db =
                    [ Right $ exAv "A" 1 []
                    , Right $ exAv "A" 2 []
                    , Right $ exAv "A" 3 []
                    , Right $ exAv "B" 1 [ExFix "A" 4]
                    ]
                  rejecting = "rejecting: A-3.0.0"
                  skipping = "skipping: A; 2.0.0, 1.0.0"
               in mkTest db "show skipping versions list" ["B"] $
                    solverFailure (\msg -> rejecting `isInfixOf` msg && skipping `isInfixOf` msg)
          , runTest $
              let db =
                    [ Left $ exInst "A" 1 "A-1.0.0" []
                    , Left $ exInst "A" 2 "A-2.0.0" []
                    , Left $ exInst "A" 3 "A-3.0.0" []
                    , Right $ exAv "B" 1 [ExFix "A" 4]
                    ]
                  rejecting = "rejecting: A-3.0.0/installed-3.0.0"
                  skipping = "skipping: A-2.0.0/installed-2.0.0, A-1.0.0/installed-1.0.0"
               in mkTest db "show skipping versions list, installed" ["B"] $
                    solverFailure (\msg -> rejecting `isInfixOf` msg && skipping `isInfixOf` msg)
          ]
      ]
  ]
  where
    indep = independentGoals
    mkvrThis = V.thisVersion . makeV
    mkvrOrEarlier = V.orEarlierVersion . makeV
    makeV v = V.mkVersion [v, 0, 0]

data GoalOrder = FixedGoalOrder | DefaultGoalOrder

{-------------------------------------------------------------------------------
  Specific example database for the tests
-------------------------------------------------------------------------------}

db1 :: ExampleDb
db1 =
  let a = exInst "A" 1 "A-1" []
   in [ Left a
      , Right $ exAv "B" 1 [ExAny "A"]
      , Right $ exAv "B" 2 [ExAny "A"]
      , Right $ exAv "C" 1 [ExFix "B" 1]
      , Right $ exAv "D" 1 [ExFix "B" 2]
      , Right $ exAv "E" 1 [ExAny "B"]
      , Right $ exAv "F" 1 [ExFix "B" 1, ExAny "E"]
      , Right $ exAv "G" 1 [ExFix "B" 2, ExAny "E"]
      , Right $ exAv "Z" 1 []
      ]

-- In this example, we _can_ install C and D as independent goals, but we have
-- to pick two different versions for B (arbitrarily)
db2 :: ExampleDb
db2 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "B" 2 [ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B", ExFix "A" 1]
  , Right $ exAv "D" 1 [ExAny "B", ExFix "A" 2]
  ]

db3 :: ExampleDb
db3 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [exFlagged "flagB" [ExFix "A" 1] [ExFix "A" 2]]
  , Right $ exAv "C" 1 [ExFix "A" 1, ExAny "B"]
  , Right $ exAv "D" 1 [ExFix "A" 2, ExAny "B"]
  ]

-- | Like db3, but the flag picks a different package rather than a
-- different package version
--
-- In db3 we cannot install C and D as independent goals because:
--
-- * The multiple instance restriction says C and D _must_ share B
-- * Since C relies on A-1, C needs B to be compiled with flagB on
-- * Since D relies on A-2, D needs B to be compiled with flagB off
-- * Hence C and D have incompatible requirements on B's flags.
--
-- However, _even_ if we don't check explicitly that we pick the same flag
-- assignment for 0.B and 1.B, we will still detect the problem because
-- 0.B depends on 0.A-1, 1.B depends on 1.A-2, hence we cannot link 0.A to
-- 1.A and therefore we cannot link 0.B to 1.B.
--
-- In db4 the situation however is trickier. We again cannot install
-- packages C and D as independent goals because:
--
-- * As above, the multiple instance restriction says that C and D _must_ share B
-- * Since C relies on Ax-2, it requires B to be compiled with flagB off
-- * Since D relies on Ay-2, it requires B to be compiled with flagB on
-- * Hence C and D have incompatible requirements on B's flags.
--
-- But now this requirement is more indirect. If we only check dependencies
-- we don't see the problem:
--
-- * We link 0.B to 1.B
-- * 0.B relies on Ay-1
-- * 1.B relies on Ax-1
--
-- We will insist that 0.Ay will be linked to 1.Ay, and 0.Ax to 1.Ax, but since
-- we only ever assign to one of these, these constraints are never broken.
db4 :: ExampleDb
db4 =
  [ Right $ exAv "Ax" 1 []
  , Right $ exAv "Ax" 2 []
  , Right $ exAv "Ay" 1 []
  , Right $ exAv "Ay" 2 []
  , Right $ exAv "B" 1 [exFlagged "flagB" [ExFix "Ax" 1] [ExFix "Ay" 1]]
  , Right $ exAv "C" 1 [ExFix "Ax" 2, ExAny "B"]
  , Right $ exAv "D" 1 [ExFix "Ay" 2, ExAny "B"]
  ]

-- | Simple database containing one package with a manual flag.
dbManualFlags :: ExampleDb
dbManualFlags =
  [ Right $
      declareFlags [ExFlag "flag" True Manual] $
        exAv "pkg" 1 [exFlagged "flag" [ExAny "true-dep"] [ExAny "false-dep"]]
  , Right $ exAv "true-dep" 1 []
  , Right $ exAv "false-dep" 1 []
  ]

-- | Database containing a setup dependency with a manual flag. A's library and
-- setup script depend on two different versions of B. B's manual flag can be
-- set to different values in the two places where it is used.
dbSetupDepWithManualFlag :: ExampleDb
dbSetupDepWithManualFlag =
  let bFlags = [ExFlag "flag" True Manual]
   in [ Right $ exAv "A" 1 [ExFix "B" 1] `withSetupDeps` [ExFix "B" 2]
      , Right $
          declareFlags bFlags $
            exAv
              "B"
              1
              [ exFlagged
                  "flag"
                  [ExAny "b-1-true-dep"]
                  [ExAny "b-1-false-dep"]
              ]
      , Right $
          declareFlags bFlags $
            exAv
              "B"
              2
              [ exFlagged
                  "flag"
                  [ExAny "b-2-true-dep"]
                  [ExAny "b-2-false-dep"]
              ]
      , Right $ exAv "b-1-true-dep" 1 []
      , Right $ exAv "b-1-false-dep" 1 []
      , Right $ exAv "b-2-true-dep" 1 []
      , Right $ exAv "b-2-false-dep" 1 []
      ]

-- | A database similar to 'dbSetupDepWithManualFlag', except that the library
-- and setup script both depend on B-1. B must be linked because of the Single
-- Instance Restriction, and its flag can only have one value.
dbLinkedSetupDepWithManualFlag :: ExampleDb
dbLinkedSetupDepWithManualFlag =
  [ Right $ exAv "A" 1 [ExFix "B" 1] `withSetupDeps` [ExFix "B" 1]
  , Right $
      declareFlags [ExFlag "flag" True Manual] $
        exAv
          "B"
          1
          [ exFlagged
              "flag"
              [ExAny "b-1-true-dep"]
              [ExAny "b-1-false-dep"]
          ]
  , Right $ exAv "b-1-true-dep" 1 []
  , Right $ exAv "b-1-false-dep" 1 []
  ]

-- | Some tests involving testsuites
--
-- Note that in this test framework test suites are always enabled; if you
-- want to test without test suites just set up a test database without
-- test suites.
--
-- * C depends on A (through its test suite)
-- * D depends on B-2 (through its test suite), but B-2 is unavailable
-- * E depends on A-1 directly and on A through its test suite. We prefer
--     to use A-1 for the test suite in this case.
-- * F depends on A-1 directly and on A-2 through its test suite. In this
--     case we currently fail to install F, although strictly speaking
--     test suites should be considered independent goals.
-- * G is like E, but for version A-2. This means that if we cannot install
--     E and G together, unless we regard them as independent goals.
db5 :: ExampleDb
db5 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 []
  , Right $ exAv "C" 1 [] `withTest` exTest "testC" [ExAny "A"]
  , Right $ exAv "D" 1 [] `withTest` exTest "testD" [ExFix "B" 2]
  , Right $ exAv "E" 1 [ExFix "A" 1] `withTest` exTest "testE" [ExAny "A"]
  , Right $ exAv "F" 1 [ExFix "A" 1] `withTest` exTest "testF" [ExFix "A" 2]
  , Right $ exAv "G" 1 [ExFix "A" 2] `withTest` exTest "testG" [ExAny "A"]
  ]

-- Now the _dependencies_ have test suites
--

-- * Installing C is a simple example. C wants version 1 of A, but depends on

--   B, and B's testsuite depends on an any version of A. In this case we prefer
--   to link (if we don't regard test suites as independent goals then of course
--   linking here doesn't even come into it).

-- * Installing [C, D] means that we prefer to link B -- depending on how we

--   set things up, this means that we should also link their test suites.
db6 :: ExampleDb
db6 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [] `withTest` exTest "testA" [ExAny "A"]
  , Right $ exAv "C" 1 [ExFix "A" 1, ExAny "B"]
  , Right $ exAv "D" 1 [ExAny "B"]
  ]

-- | This test checks that the solver can backjump to disable a flag, even if
-- the problematic dependency is also under a test suite. (issue #4390)
--
-- The goal order forces the solver to choose the flag before enabling testing.
-- Previously, the solver couldn't handle this case, because it only tried to
-- disable testing, and when that failed, it backjumped past the flag choice.
-- The solver should also try to set the flag to false, because that avoids the
-- dependency on B.
testTestSuiteWithFlag :: String -> SolverTest
testTestSuiteWithFlag name =
  goalOrder goals $
    enableAllTests $
      mkTest db name ["A", "B"] $
        solverSuccess [("A", 1), ("B", 1)]
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv "A" 1 []
            `withTest` exTest "test" [exFlagged "flag" [ExFix "B" 2] []]
      , Right $ exAv "B" 1 []
      ]

    goals :: [ExampleVar]
    goals =
      [ P QualNone "B"
      , P QualNone "A"
      , F QualNone "A" "flag"
      , S QualNone "A" TestStanzas
      ]

-- Packages with setup dependencies
--
-- Install..

-- * B: Simple example, just make sure setup deps are taken into account at all

-- * C: Both the package and the setup script depend on any version of A.

--      In this case we prefer to link

-- * D: Variation on C.1 where the package requires a specific (not latest)

--      version but the setup dependency is not fixed. Again, we prefer to
--      link (picking the older version)

-- * E: Variation on C.2 with the setup dependency the more inflexible.

--      Currently, in this case we do not see the opportunity to link because
--      we consider setup dependencies after normal dependencies; we will
--      pick A.2 for E, then realize we cannot link E.setup.A to A.2, and pick
--      A.1 instead. This isn't so easy to fix (if we want to fix it at all);
--      in particular, considering setup dependencies _before_ other deps is
--      not an improvement, because in general we would prefer to link setup
--      setups to package deps, rather than the other way around. (For example,
--      if we change this ordering then the test for D would start to install
--      two versions of A).

-- * F: The package and the setup script depend on different versions of A.

--      This will only work if setup dependencies are considered independent.
db7 :: ExampleDb
db7 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [] `withSetupDeps` [ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "A"] `withSetupDeps` [ExAny "A"]
  , Right $ exAv "D" 1 [ExFix "A" 1] `withSetupDeps` [ExAny "A"]
  , Right $ exAv "E" 1 [ExAny "A"] `withSetupDeps` [ExFix "A" 1]
  , Right $ exAv "F" 1 [ExFix "A" 2] `withSetupDeps` [ExFix "A" 1]
  ]

-- If we install C and D together (not as independent goals), we need to build
-- both B.1 and B.2, both of which depend on A.
db8 :: ExampleDb
db8 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "B" 2 [ExAny "A"]
  , Right $ exAv "C" 1 [] `withSetupDeps` [ExFix "B" 1]
  , Right $ exAv "D" 1 [] `withSetupDeps` [ExFix "B" 2]
  ]

-- Extended version of `db8` so that we have nested setup dependencies
db9 :: ExampleDb
db9 =
  db8
    ++ [ Right $ exAv "E" 1 [ExAny "C"]
       , Right $ exAv "E" 2 [ExAny "D"]
       , Right $ exAv "F" 1 [] `withSetupDeps` [ExFix "E" 1]
       , Right $ exAv "G" 1 [] `withSetupDeps` [ExFix "E" 2]
       ]

-- Multiple already-installed packages with inter-dependencies, and one package
-- (C) that depends on package A-1 for its setup script and package A-2 as a
-- library dependency.
db10 :: ExampleDb
db10 =
  let rts = exInst "rts" 1 "rts-inst" []
      ghc_prim = exInst "ghc-prim" 1 "ghc-prim-inst" [rts]
      base = exInst "base" 1 "base-inst" [rts, ghc_prim]
      a1 = exInst "A" 1 "A1-inst" [base]
      a2 = exInst "A" 2 "A2-inst" [base]
   in [ Left rts
      , Left ghc_prim
      , Left base
      , Left a1
      , Left a2
      , Right $ exAv "C" 1 [ExFix "A" 2] `withSetupDeps` [ExFix "A" 1]
      ]

-- | This database tests that a package's setup dependencies are correctly
-- linked when the package is linked. See pull request #3268.
--
-- When A and B are installed as independent goals, their dependencies on C must
-- be linked, due to the single instance restriction. Since C depends on D, 0.D
-- and 1.D must be linked. C also has a setup dependency on D, so 0.C-setup.D
-- and 1.C-setup.D must be linked. However, D's two link groups must remain
-- independent. The solver should be able to choose D-1 for C's library and D-2
-- for C's setup script.
dbSetupDeps :: ExampleDb
dbSetupDeps =
  [ Right $ exAv "A" 1 [ExAny "C"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 [ExFix "D" 1] `withSetupDeps` [ExFix "D" 2]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  ]

-- | Tests for dealing with base shims
db11 :: ExampleDb
db11 =
  let base3 = exInst "base" 3 "base-3-inst" [base4]
      base4 = exInst "base" 4 "base-4-inst" []
   in [ Left base3
      , Left base4
      , Right $ exAv "A" 1 [ExFix "base" 3]
      ]

-- | Slightly more realistic version of db11 where base-3 depends on syb
-- This means that if a package depends on base-3 and on syb, then they MUST
-- share the version of syb
--
-- * Package A relies on base-3 (which relies on base-4)
-- * Package B relies on base-4
-- * Package C relies on both A and B
-- * Package D relies on base-3 and on syb-2, which is not possible because
--     base-3 has a dependency on syb-1 (non-inheritance of the Base qualifier)
-- * Package E relies on base-4 and on syb-2, which is fine.
db12 :: ExampleDb
db12 =
  let base3 = exInst "base" 3 "base-3-inst" [base4, syb1]
      base4 = exInst "base" 4 "base-4-inst" []
      syb1 = exInst "syb" 1 "syb-1-inst" [base4]
   in [ Left base3
      , Left base4
      , Left syb1
      , Right $ exAv "syb" 2 [ExFix "base" 4]
      , Right $ exAv "A" 1 [ExFix "base" 3, ExAny "syb"]
      , Right $ exAv "B" 1 [ExFix "base" 4, ExAny "syb"]
      , Right $ exAv "C" 1 [ExAny "A", ExAny "B"]
      , Right $ exAv "D" 1 [ExFix "base" 3, ExFix "syb" 2]
      , Right $ exAv "E" 1 [ExFix "base" 4, ExFix "syb" 2]
      ]

dbBase :: ExampleDb
dbBase =
  [ Right $
      exAv
        "base"
        1
        [ExAny "ghc-prim", ExAny "integer-simple", ExAny "integer-gmp"]
  , Right $ exAv "ghc-prim" 1 []
  , Right $ exAv "integer-simple" 1 []
  , Right $ exAv "integer-gmp" 1 []
  ]

dbNonupgrade :: ExampleDb
dbNonupgrade =
  [ Left $ exInst "ghc" 1 "ghc-1" []
  , Right $ exAv "ghc" 2 []
  , Right $ exAv "ghci" 2 []
  , Right $ exAv "ghc-boot" 2 []
  , Right $ exAv "A" 1 [ExFix "ghc" 2]
  , Right $ exAv "B" 1 [ExFix "ghci" 2]
  , Right $ exAv "C" 1 [ExFix "ghc-boot" 2]
  ]

db13 :: ExampleDb
db13 =
  [ Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "A" 3 []
  ]

-- | A, B, and C have three different dependencies on D that can be set to
-- different versions with qualified constraints. Each version of D can only
-- be depended upon by one version of A, B, or C, so that the versions of A, B,
-- and C in the install plan indicate which version of D was chosen for each
-- dependency. The one-to-one correspondence between versions of A, B, and C and
-- versions of D also prevents linking, which would complicate the solver's
-- behavior.
dbConstraints :: ExampleDb
dbConstraints =
  [Right $ exAv "A" v [ExFix "D" v] | v <- [1, 4, 7]]
    ++ [Right $ exAv "B" v [] `withSetupDeps` [ExFix "D" v] | v <- [2, 5, 8]]
    ++ [Right $ exAv "C" v [] `withSetupDeps` [ExFix "D" v] | v <- [3, 6, 9]]
    ++ [Right $ exAv "D" v [] | v <- [1 .. 9]]

dbStanzaPreferences1 :: ExampleDb
dbStanzaPreferences1 =
  [ Right $ exAv "pkg" 1 [] `withTest` exTest "test" [ExAny "test-dep"]
  , Right $ exAv "test-dep" 1 []
  ]

dbStanzaPreferences2 :: ExampleDb
dbStanzaPreferences2 =
  [ Right $ exAv "pkg" 1 [] `withTest` exTest "test" [ExAny "unknown"]
  ]

-- | This is a test case for a bug in stanza preferences (#3930). The solver
-- should be able to install 'A' by enabling 'flag' and disabling testing. When
-- it tries goals in the specified order and prefers testing, it encounters
-- 'unknown-pkg2'. 'unknown-pkg2' is only introduced by testing and 'flag', so
-- the conflict set should contain both of those variables. Before the fix, it
-- only contained 'flag'. The solver backjumped past the choice to disable
-- testing and failed to find the solution.
testStanzaPreference :: String -> TestTree
testStanzaPreference name =
  let pkg =
        exAv
          "A"
          1
          [ exFlagged
              "flag"
              []
              [ExAny "unknown-pkg1"]
          ]
          `withTest` exTest
            "test"
            [ exFlagged
                "flag"
                [ExAny "unknown-pkg2"]
                []
            ]
      goals =
        [ P QualNone "A"
        , F QualNone "A" "flag"
        , S QualNone "A" TestStanzas
        ]
   in runTest $
        goalOrder goals $
          preferences [ExStanzaPref "A" [TestStanzas]] $
            mkTest [Right pkg] name ["A"] $
              solverSuccess [("A", 1)]

-- | Database with some cycles
--
-- * Simplest non-trivial cycle: A -> B and B -> A
-- * There is a cycle C -> D -> C, but it can be broken by picking the
--   right flag assignment.
db14 :: ExampleDb
db14 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "C" 1 [exFlagged "flagC" [ExAny "D"] [ExAny "E"]]
  , Right $ exAv "D" 1 [ExAny "C"]
  , Right $ exAv "E" 1 []
  ]

-- | Cycles through setup dependencies
--
-- The first cycle is unsolvable: package A has a setup dependency on B,
-- B has a regular dependency on A, and we only have a single version available
-- for both.
--
-- The second cycle can be broken by picking different versions: package C-2.0
-- has a setup dependency on D, and D has a regular dependency on C-*. However,
-- version C-1.0 is already available (perhaps it didn't have this setup dep).
-- Thus, we should be able to break this cycle even if we are installing package
-- E, which explicitly depends on C-2.0.
db15 :: ExampleDb
db15 =
  [ -- First example (real cycle, no solution)
    Right $ exAv "A" 1 [] `withSetupDeps` [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "A"]
  , -- Second example (cycle can be broken by picking versions carefully)
    Left $ exInst "C" 1 "C-1-inst" []
  , Right $ exAv "C" 2 [] `withSetupDeps` [ExAny "D"]
  , Right $ exAv "D" 1 [ExAny "C"]
  , Right $ exAv "E" 1 [ExFix "C" 2]
  ]

-- | Detect a cycle between a package and its setup script.
--
-- This type of cycle can easily occur when v2-build adds default setup
-- dependencies to packages without custom-setup stanzas. For example, cabal
-- adds 'time' as a setup dependency for 'time'. The solver should detect the
-- cycle when it attempts to link the setup and non-setup instances of the
-- package and then choose a different version for the setup dependency.
issue4161 :: String -> SolverTest
issue4161 name =
  setVerbose $
    mkTest db name ["target"] $
      SolverResult checkFullLog $
        Right [("target", 1), ("time", 1), ("time", 2)]
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "target" 1 [ExFix "time" 2]
      , Right $ exAv "time" 2 [] `withSetupDeps` [ExAny "time"]
      , Right $ exAv "time" 1 []
      ]

    checkFullLog :: [String] -> Bool
    checkFullLog =
      any $
        isInfixOf $
          "rejecting: time:setup.time~>time-2.0.0 (cyclic dependencies; "
            ++ "conflict set: time:setup.time)"

-- | Packages pkg-A, pkg-B, and pkg-C form a cycle. The solver should backtrack
-- as soon as it chooses the last package in the cycle, to avoid searching parts
-- of the tree that have no solution. Since there is no way to break the cycle,
-- it should fail with an error message describing the cycle.
testCyclicDependencyErrorMessages :: String -> SolverTest
testCyclicDependencyErrorMessages name =
  goalOrder goals $
    mkTest db name ["pkg-A"] $
      SolverResult checkFullLog $
        Left checkSummarizedLog
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "pkg-A" 1 [ExAny "pkg-B"]
      , Right $ exAv "pkg-B" 1 [ExAny "pkg-C"]
      , Right $ exAv "pkg-C" 1 [ExAny "pkg-A", ExAny "pkg-D"]
      , Right $ exAv "pkg-D" 1 [ExAny "pkg-E"]
      , Right $ exAv "pkg-E" 1 []
      ]

    -- The solver should backtrack as soon as pkg-A, pkg-B, and pkg-C form a
    -- cycle. It shouldn't try pkg-D or pkg-E.
    checkFullLog :: [String] -> Bool
    checkFullLog =
      not . any (\l -> "pkg-D" `isInfixOf` l || "pkg-E" `isInfixOf` l)

    checkSummarizedLog :: String -> Bool
    checkSummarizedLog =
      isInfixOf "rejecting: pkg-C-1.0.0 (cyclic dependencies; conflict set: pkg-A, pkg-B, pkg-C)"

    -- Solve for pkg-D and pkg-E last.
    goals :: [ExampleVar]
    goals = [P QualNone ("pkg-" ++ [c]) | c <- ['A' .. 'E']]

-- | Check that the solver can backtrack after encountering the SIR (issue #2843)
--
-- When A and B are installed as independent goals, the single instance
-- restriction prevents B from depending on C.  This database tests that the
-- solver can backtrack after encountering the single instance restriction and
-- choose the only valid flag assignment (-flagA +flagB):
--
-- > flagA flagB  B depends on
-- >  On    _     C-*
-- >  Off   On    E-*               <-- only valid flag assignment
-- >  Off   Off   D-2.0, C-*
--
-- Since A depends on C-* and D-1.0, and C-1.0 depends on any version of D,
-- we must build C-1.0 against D-1.0. Since B depends on D-2.0, we cannot have
-- C in the transitive closure of B's dependencies, because that would mean we
-- would need two instances of C: one built against D-1.0 and one built against
-- D-2.0.
db16 :: ExampleDb
db16 =
  [ Right $ exAv "A" 1 [ExAny "C", ExFix "D" 1]
  , Right $
      exAv
        "B"
        1
        [ ExFix "D" 2
        , exFlagged
            "flagA"
            [ExAny "C"]
            [ exFlagged
                "flagB"
                [ExAny "E"]
                [ExAny "C"]
            ]
        ]
  , Right $ exAv "C" 1 [ExAny "D"]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  , Right $ exAv "E" 1 []
  ]

-- Try to get the solver to backtrack while satisfying
-- reject-unconstrained-dependencies: both the first and last versions of A
-- require packages outside the closed set, so it will have to try the
-- middle one.
db17 :: ExampleDb
db17 =
  [ Right $ exAv "A" 1 [ExAny "C"]
  , Right $ exAv "A" 2 [ExAny "B"]
  , Right $ exAv "A" 3 [ExAny "C"]
  , Right $ exAv "B" 1 []
  , Right $ exAv "C" 1 [ExAny "B"]
  ]

-- | This test checks that when the solver discovers a constraint on a
-- package's version after choosing to link that package, it can backtrack to
-- try alternative versions for the linked-to package. See pull request #3327.
--
-- When A and B are installed as independent goals, their dependencies on C
-- must be linked. Since C depends on D, A and B's dependencies on D must also
-- be linked. This test fixes the goal order so that the solver chooses D-2 for
-- both 0.D and 1.D before it encounters the test suites' constraints. The
-- solver must backtrack to try D-1 for both 0.D and 1.D.
testIndepGoals2 :: String -> SolverTest
testIndepGoals2 name =
  goalOrder goals $
    independentGoals $
      enableAllTests $
        mkTest db name ["A", "B"] $
          solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1)]
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "A" 1 [ExAny "C"] `withTest` exTest "test" [ExFix "D" 1]
      , Right $ exAv "B" 1 [ExAny "C"] `withTest` exTest "test" [ExFix "D" 1]
      , Right $ exAv "C" 1 [ExAny "D"]
      , Right $ exAv "D" 1 []
      , Right $ exAv "D" 2 []
      ]

    goals :: [ExampleVar]
    goals =
      [ P (QualIndep "A") "A"
      , P (QualIndep "A") "C"
      , P (QualIndep "A") "D"
      , P (QualIndep "B") "B"
      , P (QualIndep "B") "C"
      , P (QualIndep "B") "D"
      , S (QualIndep "B") "B" TestStanzas
      , S (QualIndep "A") "A" TestStanzas
      ]

-- | Issue #2834
-- When both A and B are installed as independent goals, their dependencies on
-- C must be linked. The only combination of C's flags that is consistent with
-- A and B's dependencies on D is -flagA +flagB. This database tests that the
-- solver can backtrack to find the right combination of flags (requiring F, but
-- not E or G) and apply it to both 0.C and 1.C.
--
-- > flagA flagB  C depends on
-- >  On    _     D-1, E-*
-- >  Off   On    F-*        <-- Only valid choice
-- >  Off   Off   D-2, G-*
--
-- The single instance restriction means we cannot have one instance of C
-- built against D-1 and one instance built against D-2; since A depends on
-- D-1, and B depends on C-2, it is therefore important that C cannot depend
-- on any version of D.
db18 :: ExampleDb
db18 =
  [ Right $ exAv "A" 1 [ExAny "C", ExFix "D" 1]
  , Right $ exAv "B" 1 [ExAny "C", ExFix "D" 2]
  , Right $
      exAv
        "C"
        1
        [ exFlagged
            "flagA"
            [ExFix "D" 1, ExAny "E"]
            [ exFlagged
                "flagB"
                [ExAny "F"]
                [ExFix "D" 2, ExAny "G"]
            ]
        ]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  , Right $ exAv "E" 1 []
  , Right $ exAv "F" 1 []
  , Right $ exAv "G" 1 []
  ]

-- | When both values for flagA introduce package B, the solver should be able
-- to choose B before choosing a value for flagA. It should try to choose a
-- version for B that is in the union of the version ranges required by +flagA
-- and -flagA.
commonDependencyLogMessage :: String -> SolverTest
commonDependencyLogMessage name =
  mkTest db name ["A"] $
    solverFailure $
      isInfixOf $
        "[__0] trying: A-1.0.0 (user goal)\n"
          ++ "[__1] next goal: B (dependency of A +/-flagA)\n"
          ++ "[__1] rejecting: B-2.0.0 (conflict: A +/-flagA => B==1.0.0 || ==3.0.0)"
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ exFlagged
                "flagA"
                [ExFix "B" 1]
                [ExFix "B" 3]
            ]
      , Right $ exAv "B" 2 []
      ]

-- | Test lifting dependencies out of multiple levels of conditionals.
twoLevelDeepCommonDependencyLogMessage :: String -> SolverTest
twoLevelDeepCommonDependencyLogMessage name =
  mkTest db name ["A"] $
    solverFailure $
      isInfixOf $
        "unknown package: B (dependency of A +/-flagA +/-flagB)"
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ exFlagged
                "flagA"
                [ exFlagged
                    "flagB"
                    [ExAny "B"]
                    [ExAny "B"]
                ]
                [ exFlagged
                    "flagB"
                    [ExAny "B"]
                    [ExAny "B"]
                ]
            ]
      ]

-- | Test handling nested conditionals that are controlled by the same flag.
-- The solver should treat flagA as introducing 'unknown' with value true, not
-- both true and false. That means that when +flagA causes a conflict, the
-- solver should try flipping flagA to false to resolve the conflict, rather
-- than backjumping past flagA.
testBackjumpingWithCommonDependency :: String -> SolverTest
testBackjumpingWithCommonDependency name =
  mkTest db name ["A"] $ solverSuccess [("A", 1), ("B", 1)]
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ exFlagged
                "flagA"
                [ exFlagged
                    "flagA"
                    [ExAny "unknown"]
                    [ExAny "unknown"]
                ]
                [ExAny "B"]
            ]
      , Right $ exAv "B" 1 []
      ]

-- | Tricky test case with independent goals (issue #2842)
--
-- Suppose we are installing D, E, and F as independent goals:
--
-- * D depends on A-* and C-1, requiring A-1 to be built against C-1
-- * E depends on B-* and C-2, requiring B-1 to be built against C-2
-- * F depends on A-* and B-*; this means we need A-1 and B-1 both to be built
--     against the same version of C, violating the single instance restriction.
--
-- We can visualize this DB as:
--
-- >    C-1   C-2
-- >    /|\   /|\
-- >   / | \ / | \
-- >  /  |  X  |  \
-- > |   | / \ |   |
-- > |   |/   \|   |
-- > |   +     +   |
-- > |   |     |   |
-- > |   A     B   |
-- >  \  |\   /|  /
-- >   \ | \ / | /
-- >    \|  V  |/
-- >     D  F  E
testIndepGoals3 :: String -> SolverTest
testIndepGoals3 name =
  goalOrder goals $
    independentGoals $
      mkTest db name ["D", "E", "F"] anySolverFailure
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "A" 1 [ExAny "C"]
      , Right $ exAv "B" 1 [ExAny "C"]
      , Right $ exAv "C" 1 []
      , Right $ exAv "C" 2 []
      , Right $ exAv "D" 1 [ExAny "A", ExFix "C" 1]
      , Right $ exAv "E" 1 [ExAny "B", ExFix "C" 2]
      , Right $ exAv "F" 1 [ExAny "A", ExAny "B"]
      ]

    goals :: [ExampleVar]
    goals =
      [ P (QualIndep "D") "D"
      , P (QualIndep "D") "C"
      , P (QualIndep "D") "A"
      , P (QualIndep "E") "E"
      , P (QualIndep "E") "C"
      , P (QualIndep "E") "B"
      , P (QualIndep "F") "F"
      , P (QualIndep "F") "B"
      , P (QualIndep "F") "C"
      , P (QualIndep "F") "A"
      ]

-- | This test checks that the solver correctly backjumps when dependencies
-- of linked packages are not linked. It is an example where the conflict set
-- from enforcing the single instance restriction is not sufficient. See pull
-- request #3327.
--
-- When A, B, and C are installed as independent goals with the specified goal
-- order, the first choice that the solver makes for E is 0.E-2. Then, when it
-- chooses dependencies for B and C, it links both 1.E and 2.E to 0.E. Finally,
-- the solver discovers C's test's constraint on E. It must backtrack to try
-- 1.E-1 and then link 2.E to 1.E. Backjumping all the way to 0.E does not lead
-- to a solution, because 0.E's version is constrained by A and cannot be
-- changed.
testIndepGoals4 :: String -> SolverTest
testIndepGoals4 name =
  goalOrder goals $
    independentGoals $
      enableAllTests $
        mkTest db name ["A", "B", "C"] $
          solverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("E", 1), ("E", 2)]
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "A" 1 [ExFix "E" 2]
      , Right $ exAv "B" 1 [ExAny "D"]
      , Right $ exAv "C" 1 [ExAny "D"] `withTest` exTest "test" [ExFix "E" 1]
      , Right $ exAv "D" 1 [ExAny "E"]
      , Right $ exAv "E" 1 []
      , Right $ exAv "E" 2 []
      ]

    goals :: [ExampleVar]
    goals =
      [ P (QualIndep "A") "A"
      , P (QualIndep "A") "E"
      , P (QualIndep "B") "B"
      , P (QualIndep "B") "D"
      , P (QualIndep "B") "E"
      , P (QualIndep "C") "C"
      , P (QualIndep "C") "D"
      , P (QualIndep "C") "E"
      , S (QualIndep "C") "C" TestStanzas
      ]

-- | Test the trace messages that we get when a package refers to an unknown pkg
--
-- TODO: Currently we don't actually test the trace messages, and this particular
-- test still succeeds. The trace can only be verified by hand.
db21 :: ExampleDb
db21 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "A" 2 [ExAny "C"] -- A-2.0 will be tried first, but C unknown
  , Right $ exAv "B" 1 []
  ]

-- | A variant of 'db21', which actually fails.
db22 :: ExampleDb
db22 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "A" 2 [ExAny "C"]
  ]

-- | Another test for the unknown package message.  This database tests that
-- filtering out redundant conflict set messages in the solver log doesn't
-- interfere with generating a message about a missing package (part of issue
-- #3617). The conflict set for the missing package is {A, B}. That conflict set
-- is propagated up the tree to the level of A. Since the conflict set is the
-- same at both levels, the solver only keeps one of the backjumping messages.
db23 :: ExampleDb
db23 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  ]

-- | Database for (unsuccessfully) trying to expose a bug in the handling
-- of implied linking constraints. The question is whether an implied linking
-- constraint should only have the introducing package in its conflict set,
-- or also its link target.
--
-- It turns out that as long as the Single Instance Restriction is in place,
-- it does not matter, because there will always be an option that is failing
-- due to the SIR, which contains the link target in its conflict set.
--
-- Even if the SIR is not in place, if there is a solution, one will always
-- be found, because without the SIR, linking is always optional, but never
-- necessary.
testIndepGoals5 :: String -> GoalOrder -> SolverTest
testIndepGoals5 name fixGoalOrder =
  case fixGoalOrder of
    FixedGoalOrder -> goalOrder goals test
    DefaultGoalOrder -> test
  where
    test :: SolverTest
    test =
      independentGoals $
        mkTest db name ["X", "Y"] $
          solverSuccess
            [("A", 1), ("A", 2), ("B", 1), ("C", 1), ("C", 2), ("X", 1), ("Y", 1)]

    db :: ExampleDb
    db =
      [ Right $ exAv "X" 1 [ExFix "C" 2, ExAny "A"]
      , Right $ exAv "Y" 1 [ExFix "C" 1, ExFix "A" 2]
      , Right $ exAv "A" 1 []
      , Right $ exAv "A" 2 [ExAny "B"]
      , Right $ exAv "B" 1 [ExAny "C"]
      , Right $ exAv "C" 1 []
      , Right $ exAv "C" 2 []
      ]

    goals :: [ExampleVar]
    goals =
      [ P (QualIndep "X") "X"
      , P (QualIndep "X") "A"
      , P (QualIndep "X") "B"
      , P (QualIndep "X") "C"
      , P (QualIndep "Y") "Y"
      , P (QualIndep "Y") "A"
      , P (QualIndep "Y") "B"
      , P (QualIndep "Y") "C"
      ]

-- | A simplified version of 'testIndepGoals5'.
testIndepGoals6 :: String -> GoalOrder -> SolverTest
testIndepGoals6 name fixGoalOrder =
  case fixGoalOrder of
    FixedGoalOrder -> goalOrder goals test
    DefaultGoalOrder -> test
  where
    test :: SolverTest
    test =
      independentGoals $
        mkTest db name ["X", "Y"] $
          solverSuccess
            [("A", 1), ("A", 2), ("B", 1), ("B", 2), ("X", 1), ("Y", 1)]

    db :: ExampleDb
    db =
      [ Right $ exAv "X" 1 [ExFix "B" 2, ExAny "A"]
      , Right $ exAv "Y" 1 [ExFix "B" 1, ExFix "A" 2]
      , Right $ exAv "A" 1 []
      , Right $ exAv "A" 2 [ExAny "B"]
      , Right $ exAv "B" 1 []
      , Right $ exAv "B" 2 []
      ]

    goals :: [ExampleVar]
    goals =
      [ P (QualIndep "X") "X"
      , P (QualIndep "X") "A"
      , P (QualIndep "X") "B"
      , P (QualIndep "Y") "Y"
      , P (QualIndep "Y") "A"
      , P (QualIndep "Y") "B"
      ]

dbExts1 :: ExampleDb
dbExts1 =
  [ Right $ exAv "A" 1 [ExExt (EnableExtension RankNTypes)]
  , Right $ exAv "B" 1 [ExExt (EnableExtension CPP), ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B"]
  , Right $ exAv "D" 1 [ExExt (DisableExtension CPP), ExAny "B"]
  , Right $ exAv "E" 1 [ExExt (UnknownExtension "custom"), ExAny "C"]
  ]

dbLangs1 :: ExampleDb
dbLangs1 =
  [ Right $ exAv "A" 1 [ExLang Haskell2010]
  , Right $ exAv "B" 1 [ExLang Haskell98, ExAny "A"]
  , Right $ exAv "C" 1 [ExLang (UnknownLanguage "Haskell3000"), ExAny "B"]
  ]

-- | cabal must set enable-exe to false in order to avoid the unavailable
-- dependency. Flags are true by default. The flag choice causes "pkg" to
-- depend on "false-dep".
testBuildable :: String -> ExampleDependency -> TestTree
testBuildable testName unavailableDep =
  runTest $
    mkTestExtLangPC (Just []) (Just [Haskell98]) (Just []) db testName ["pkg"] expected
  where
    expected = solverSuccess [("false-dep", 1), ("pkg", 1)]
    db =
      [ Right $
          exAv
            "pkg"
            1
            [ exFlagged
                "enable-exe"
                [ExAny "true-dep"]
                [ExAny "false-dep"]
            ]
            `withExe` exExe
              "exe"
              [ unavailableDep
              , ExFlagged "enable-exe" (dependencies []) unbuildableDependencies
              ]
      , Right $ exAv "true-dep" 1 []
      , Right $ exAv "false-dep" 1 []
      ]

-- | cabal must choose -flag1 +flag2 for "pkg", which requires packages
-- "flag1-false" and "flag2-true".
dbBuildable1 :: ExampleDb
dbBuildable1 =
  [ Right $
      exAv
        "pkg"
        1
        [ exFlagged "flag1" [ExAny "flag1-true"] [ExAny "flag1-false"]
        , exFlagged "flag2" [ExAny "flag2-true"] [ExAny "flag2-false"]
        ]
        `withExes` [ exExe
                      "exe1"
                      [ ExAny "unknown"
                      , ExFlagged "flag1" (dependencies []) unbuildableDependencies
                      , ExFlagged "flag2" (dependencies []) unbuildableDependencies
                      ]
                   , exExe
                      "exe2"
                      [ ExAny "unknown"
                      , ExFlagged
                          "flag1"
                          (dependencies [])
                          (dependencies [ExFlagged "flag2" unbuildableDependencies (dependencies [])])
                      ]
                   ]
  , Right $ exAv "flag1-true" 1 []
  , Right $ exAv "flag1-false" 1 []
  , Right $ exAv "flag2-true" 1 []
  , Right $ exAv "flag2-false" 1 []
  ]

-- | cabal must pick B-2 to avoid the unknown dependency.
dbBuildable2 :: ExampleDb
dbBuildable2 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "unknown"]
  , Right $
      exAv "B" 2 []
        `withExe` exExe
          "exe"
          [ ExAny "unknown"
          , ExFlagged "disable-exe" unbuildableDependencies (dependencies [])
          ]
  , Right $ exAv "B" 3 [ExAny "unknown"]
  ]

-- | Package databases for testing @pkg-config@ dependencies.
-- when no pkgconfig db is present, cabal must pick flag1 false and flag2 true to avoid the pkg dependency.
dbPC1 :: ExampleDb
dbPC1 =
  [ Right $ exAv "A" 1 [ExPkg ("pkgA", 1)]
  , Right $ exAv "B" 1 [ExPkg ("pkgB", 1), ExAny "A"]
  , Right $ exAv "B" 2 [ExPkg ("pkgB", 2), ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B"]
  , Right $ exAv "D" 1 [exFlagged "flag1" [ExAny "A"] [], exFlagged "flag2" [] [ExAny "A"]]
  ]

-- | Test for the solver's summarized log. The final conflict set is {A, F},
-- though the goal order forces the solver to find the (avoidable) conflict
-- between B and C first. When the solver reaches the backjump limit, it should
-- only show the log to the first conflict. When the backjump limit is high
-- enough to allow an exhaustive search, the solver should make use of the final
-- conflict set to only show the conflict between A and F in the summarized log.
testSummarizedLog :: String -> Maybe Int -> String -> TestTree
testSummarizedLog testName mbj expectedMsg =
  runTest $
    maxBackjumps mbj $
      goalOrder goals $
        mkTest db testName ["A"] $
          solverFailure (== expectedMsg)
  where
    db =
      [ Right $ exAv "A" 1 [ExAny "B", ExAny "F"]
      , Right $ exAv "B" 3 [ExAny "C"]
      , Right $ exAv "B" 2 [ExAny "D"]
      , Right $ exAv "B" 1 [ExAny "E"]
      , Right $ exAv "E" 1 []
      ]

    goals :: [ExampleVar]
    goals = [P QualNone pkg | pkg <- ["A", "B", "C", "D", "E", "F"]]

dbMinimizeConflictSet :: ExampleDb
dbMinimizeConflictSet =
  [ Right $ exAv "A" 3 [ExFix "B" 2, ExFix "C" 1, ExFix "D" 2]
  , Right $ exAv "A" 2 [ExFix "B" 1, ExFix "C" 2, ExFix "D" 2]
  , Right $ exAv "A" 1 [ExFix "B" 1, ExFix "C" 1, ExFix "D" 2]
  , Right $ exAv "B" 1 []
  , Right $ exAv "C" 1 []
  , Right $ exAv "D" 1 []
  ]

-- | Test that the solver can find a minimal conflict set with
-- --minimize-conflict-set. In the first run, the goal order causes the solver
-- to find that A-3 conflicts with B, A-2 conflicts with C, and A-1 conflicts
-- with D. The full log should show that the original final conflict set is
-- {A, B, C, D}. Then the solver should be able to reduce the conflict set to
-- {A, D}, since all versions of A conflict with D. The summarized log should
-- only mention A and D.
testMinimizeConflictSet :: String -> TestTree
testMinimizeConflictSet testName =
  runTest $
    minimizeConflictSet $
      goalOrder goals $
        setVerbose $
          mkTest dbMinimizeConflictSet testName ["A"] $
            SolverResult checkFullLog (Left (== expectedMsg))
  where
    checkFullLog :: [String] -> Bool
    checkFullLog =
      containsInOrder
        [ "[__0] fail (backjumping, conflict set: A, B, C, D)"
        , "Found no solution after exhaustively searching the dependency tree. "
            ++ "Rerunning the dependency solver to minimize the conflict set ({A, B, C, D})."
        , "Trying to remove variable \"A\" from the conflict set."
        , "Failed to remove \"A\" from the conflict set. Continuing with {A, B, C, D}."
        , "Trying to remove variable \"B\" from the conflict set."
        , "Successfully removed \"B\" from the conflict set. Continuing with {A, D}."
        , "Trying to remove variable \"D\" from the conflict set."
        , "Failed to remove \"D\" from the conflict set. Continuing with {A, D}."
        ]

    expectedMsg =
      "Could not resolve dependencies:\n"
        ++ "[__0] trying: A-3.0.0 (user goal)\n"
        ++ "[__1] next goal: D (dependency of A)\n"
        ++ "[__1] rejecting: D-1.0.0 (conflict: A => D==2.0.0)\n"
        ++ "[__1] fail (backjumping, conflict set: A, D)\n"
        ++ "After searching the rest of the dependency tree exhaustively, these "
        ++ "were the goals I've had most trouble fulfilling: A (5), D (4)"

    goals :: [ExampleVar]
    goals = [P QualNone pkg | pkg <- ["A", "B", "C", "D"]]

-- | This test uses the same packages and goal order as testMinimizeConflictSet,
-- but it doesn't set --minimize-conflict-set. The solver should print the
-- original final conflict set and the conflict between A and B. It should also
-- suggest rerunning with --minimize-conflict-set.
testNoMinimizeConflictSet :: String -> TestTree
testNoMinimizeConflictSet testName =
  runTest $
    goalOrder goals $
      setVerbose $
        mkTest dbMinimizeConflictSet testName ["A"] $
          solverFailure (== expectedMsg)
  where
    expectedMsg =
      "Could not resolve dependencies:\n"
        ++ "[__0] trying: A-3.0.0 (user goal)\n"
        ++ "[__1] next goal: B (dependency of A)\n"
        ++ "[__1] rejecting: B-1.0.0 (conflict: A => B==2.0.0)\n"
        ++ "[__1] fail (backjumping, conflict set: A, B)\n"
        ++ "After searching the rest of the dependency tree exhaustively, "
        ++ "these were the goals I've had most trouble fulfilling: "
        ++ "A (7), B (2), C (2), D (2)\n"
        ++ "Try running with --minimize-conflict-set to improve the error message."

    goals :: [ExampleVar]
    goals = [P QualNone pkg | pkg <- ["A", "B", "C", "D"]]

{-------------------------------------------------------------------------------
  Simple databases for the illustrations for the backjumping blog post
-------------------------------------------------------------------------------}

-- | Motivate conflict sets
dbBJ1a :: ExampleDb
dbBJ1a =
  [ Right $ exAv "A" 1 [ExFix "B" 1]
  , Right $ exAv "A" 2 [ExFix "B" 2]
  , Right $ exAv "B" 1 []
  ]

-- | Show that we can skip some decisions
dbBJ1b :: ExampleDb
dbBJ1b =
  [ Right $ exAv "A" 1 [ExFix "B" 1]
  , Right $ exAv "A" 2 [ExFix "B" 2, ExAny "C"]
  , Right $ exAv "B" 1 []
  , Right $ exAv "C" 1 []
  , Right $ exAv "C" 2 []
  ]

-- | Motivate why both A and B need to be in the conflict set
dbBJ1c :: ExampleDb
dbBJ1c =
  [ Right $ exAv "A" 1 [ExFix "B" 1]
  , Right $ exAv "B" 1 []
  , Right $ exAv "B" 2 []
  ]

-- | Motivate the need for accumulating conflict sets while we walk the tree
dbBJ2 :: ExampleDb
dbBJ2 =
  [ Right $ exAv "A" 1 [ExFix "B" 1]
  , Right $ exAv "A" 2 [ExFix "B" 2]
  , Right $ exAv "B" 1 [ExFix "C" 1]
  , Right $ exAv "B" 2 [ExFix "C" 2]
  , Right $ exAv "C" 1 []
  ]

-- | Motivate the need for `QGoalReason`
dbBJ3 :: ExampleDb
dbBJ3 =
  [ Right $ exAv "A" 1 [ExAny "Ba"]
  , Right $ exAv "A" 2 [ExAny "Bb"]
  , Right $ exAv "Ba" 1 [ExFix "C" 1]
  , Right $ exAv "Bb" 1 [ExFix "C" 2]
  , Right $ exAv "C" 1 []
  ]

-- | `QGOalReason` not unique
dbBJ4 :: ExampleDb
dbBJ4 =
  [ Right $ exAv "A" 1 [ExAny "B", ExAny "C"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 []
  ]

-- | Flags are represented somewhat strangely in the tree
--
-- This example probably won't be in the blog post itself but as a separate
-- bug report (#3409)
dbBJ5 :: ExampleDb
dbBJ5 =
  [ Right $ exAv "A" 1 [exFlagged "flagA" [ExFix "B" 1] [ExFix "C" 1]]
  , Right $ exAv "B" 1 [ExFix "D" 1]
  , Right $ exAv "C" 1 [ExFix "D" 2]
  , Right $ exAv "D" 1 []
  ]

-- | Conflict sets for cycles
dbBJ6 :: ExampleDb
dbBJ6 =
  [ Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 []
  , Right $ exAv "B" 2 [ExAny "C"]
  , Right $ exAv "C" 1 [ExAny "A"]
  ]

-- | Conflicts not unique
dbBJ7 :: ExampleDb
dbBJ7 =
  [ Right $ exAv "A" 1 [ExAny "B", ExFix "C" 1]
  , Right $ exAv "B" 1 [ExFix "C" 1]
  , Right $ exAv "C" 1 []
  , Right $ exAv "C" 2 []
  ]

-- | Conflict sets for SIR (C shared subgoal of independent goals A, B)
dbBJ8 :: ExampleDb
dbBJ8 =
  [ Right $ exAv "A" 1 [ExAny "C"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 []
  ]

{-------------------------------------------------------------------------------
  Databases for build-tool-depends
-------------------------------------------------------------------------------}

-- | Multiple packages depending on exes from 'bt-pkg'.
dbBuildTools :: ExampleDb
dbBuildTools =
  [ Right $ exAv "A" 1 [ExBuildToolAny "bt-pkg" "exe1"]
  , Right $
      exAv
        "B"
        1
        [ exFlagged
            "flagB"
            [ExAny "unknown"]
            [ExBuildToolAny "bt-pkg" "exe1"]
        ]
  , Right $ exAv "C" 1 [] `withTest` exTest "testC" [ExBuildToolAny "bt-pkg" "exe1"]
  , Right $ exAv "D" 1 [ExBuildToolAny "bt-pkg" "unknown-exe"]
  , Right $ exAv "E" 1 [ExBuildToolAny "unknown-pkg" "exe1"]
  , Right $
      exAv
        "F"
        1
        [ exFlagged
            "flagF"
            [ExBuildToolAny "bt-pkg" "unknown-exe"]
            [ExAny "unknown"]
        ]
  , Right $ exAv "G" 1 [] `withTest` exTest "testG" [ExBuildToolAny "bt-pkg" "unknown-exe"]
  , Right $ exAv "H" 1 [ExBuildToolFix "bt-pkg" "exe1" 3]
  , Right $ exAv "bt-pkg" 4 []
  , Right $ exAv "bt-pkg" 3 [] `withExe` exExe "exe2" []
  , Right $ exAv "bt-pkg" 2 [] `withExe` exExe "exe1" []
  , Right $ exAv "bt-pkg" 1 []
  ]

-- The solver should never choose an installed package for a build tool
-- dependency.
rejectInstalledBuildToolPackage :: String -> SolverTest
rejectInstalledBuildToolPackage name =
  mkTest db name ["A"] $
    solverFailure $
      isInfixOf $
        "rejecting: A:B:exe.B-1.0.0/installed-1 "
          ++ "(does not contain executable 'exe', which is required by A)"
  where
    db :: ExampleDb
    db =
      [ Right $ exAv "A" 1 [ExBuildToolAny "B" "exe"]
      , Left $ exInst "B" 1 "B-1" []
      ]

-- | This test forces the solver to choose B as a build-tool dependency before
-- it sees the dependency on executable exe2 from B. The solver needs to check
-- that the version that it already chose for B contains the necessary
-- executable. This order causes a different "missing executable" error message
-- than when the solver checks for the executable in the same step that it
-- chooses the build-tool package.
--
-- This case may become impossible if we ever add the executable name to the
-- build-tool goal qualifier. Then this test would involve two qualified goals
-- for B, one for exe1 and another for exe2.
chooseExeAfterBuildToolsPackage :: Bool -> String -> SolverTest
chooseExeAfterBuildToolsPackage shouldSucceed name =
  goalOrder goals $
    mkTest db name ["A"] $
      if shouldSucceed
        then solverSuccess [("A", 1), ("B", 1)]
        else
          solverFailure $
            isInfixOf $
              "rejecting: A:+flagA (requires executable 'exe2' from A:B:exe.B, "
                ++ "but the component does not exist)"
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ ExBuildToolAny "B" "exe1"
            , exFlagged
                "flagA"
                [ExBuildToolAny "B" "exe2"]
                [ExAny "unknown"]
            ]
      , Right $
          exAv "B" 1 []
            `withExes` [exExe exe [] | exe <- if shouldSucceed then ["exe1", "exe2"] else ["exe1"]]
      ]

    goals :: [ExampleVar]
    goals =
      [ P QualNone "A"
      , P (QualExe "A" "B") "B"
      , F QualNone "A" "flagA"
      ]

-- | Test that when one package depends on two executables from another package,
-- both executables must come from the same instance of that package. We could
-- lift this restriction in the future by adding the executable name to the goal
-- qualifier.
requireConsistentBuildToolVersions :: String -> SolverTest
requireConsistentBuildToolVersions name =
  mkTest db name ["A"] $
    solverFailure $
      isInfixOf $
        "[__1] rejecting: A:B:exe.B-2.0.0 (conflict: A => A:B:exe.B (exe exe1)==1.0.0)\n"
          ++ "[__1] rejecting: A:B:exe.B-1.0.0 (conflict: A => A:B:exe.B (exe exe2)==2.0.0)"
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ ExBuildToolFix "B" "exe1" 1
            , ExBuildToolFix "B" "exe2" 2
            ]
      , Right $ exAv "B" 2 [] `withExes` exes
      , Right $ exAv "B" 1 [] `withExes` exes
      ]

    exes = [exExe "exe1" [], exExe "exe2" []]

-- | This test is similar to the failure case for
-- chooseExeAfterBuildToolsPackage, except that the build tool is unbuildable
-- instead of missing.
chooseUnbuildableExeAfterBuildToolsPackage :: String -> SolverTest
chooseUnbuildableExeAfterBuildToolsPackage name =
  constraints [ExFlagConstraint (ScopeAnyQualifier "B") "build-bt2" False] $
    goalOrder goals $
      mkTest db name ["A"] $
        solverFailure $
          isInfixOf $
            "rejecting: A:+use-bt2 (requires executable 'bt2' from A:B:exe.B, but "
              ++ "the component is not buildable in the current environment)"
  where
    db :: ExampleDb
    db =
      [ Right $
          exAv
            "A"
            1
            [ ExBuildToolAny "B" "bt1"
            , exFlagged
                "use-bt2"
                [ExBuildToolAny "B" "bt2"]
                [ExAny "unknown"]
            ]
      , Right $
          exAvNoLibrary "B" 1
            `withExes` [ exExe "bt1" []
                       , exExe "bt2" [ExFlagged "build-bt2" (dependencies []) unbuildableDependencies]
                       ]
      ]

    goals :: [ExampleVar]
    goals =
      [ P QualNone "A"
      , P (QualExe "A" "B") "B"
      , F QualNone "A" "use-bt2"
      ]

{-------------------------------------------------------------------------------
  Databases for legacy build-tools
-------------------------------------------------------------------------------}
dbLegacyBuildTools1 :: ExampleDb
dbLegacyBuildTools1 =
  [ Right $ exAv "alex" 1 [] `withExe` exExe "alex" []
  , Right $ exAv "A" 1 [ExLegacyBuildToolAny "alex"]
  ]

-- Test that a recognized build tool dependency specifies the name of both the
-- package and the executable. This db has no solution.
dbLegacyBuildTools2 :: ExampleDb
dbLegacyBuildTools2 =
  [ Right $ exAv "alex" 1 [] `withExe` exExe "other-exe" []
  , Right $ exAv "other-package" 1 [] `withExe` exExe "alex" []
  , Right $ exAv "A" 1 [ExLegacyBuildToolAny "alex"]
  ]

-- Test that build-tools on a random thing doesn't matter (only
-- the ones we recognize need to be in db)
dbLegacyBuildTools3 :: ExampleDb
dbLegacyBuildTools3 =
  [ Right $ exAv "A" 1 [ExLegacyBuildToolAny "otherdude"]
  ]

-- Test that we can solve for different versions of executables
dbLegacyBuildTools4 :: ExampleDb
dbLegacyBuildTools4 =
  [ Right $ exAv "alex" 1 [] `withExe` exExe "alex" []
  , Right $ exAv "alex" 2 [] `withExe` exExe "alex" []
  , Right $ exAv "A" 1 [ExLegacyBuildToolFix "alex" 1]
  , Right $ exAv "B" 1 [ExLegacyBuildToolFix "alex" 2]
  , Right $ exAv "C" 1 [ExAny "A", ExAny "B"]
  ]

-- Test that exe is not related to library choices
dbLegacyBuildTools5 :: ExampleDb
dbLegacyBuildTools5 =
  [ Right $ exAv "alex" 1 [ExFix "A" 1] `withExe` exExe "alex" []
  , Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [ExLegacyBuildToolFix "alex" 1, ExFix "A" 2]
  ]

-- Test that build-tools on build-tools works
dbLegacyBuildTools6 :: ExampleDb
dbLegacyBuildTools6 =
  [ Right $ exAv "alex" 1 [] `withExe` exExe "alex" []
  , Right $ exAv "happy" 1 [ExLegacyBuildToolAny "alex"] `withExe` exExe "happy" []
  , Right $ exAv "A" 1 [ExLegacyBuildToolAny "happy"]
  ]

-- Test that build-depends on library/executable package works.
-- Extracted from https://github.com/haskell/cabal/issues/3775
dbIssue3775 :: ExampleDb
dbIssue3775 =
  [ Right $ exAv "warp" 1 []
  , -- NB: the warp build-depends refers to the package, not the internal
    -- executable!
    Right $ exAv "A" 2 [ExFix "warp" 1] `withExe` exExe "warp" [ExAny "A"]
  , Right $ exAv "B" 2 [ExAny "A", ExAny "warp"]
  ]

-- | Returns true if the second list contains all elements of the first list, in
-- order.
containsInOrder :: Eq a => [a] -> [a] -> Bool
containsInOrder [] _ = True
containsInOrder _ [] = False
containsInOrder (x : xs) (y : ys)
  | x == y = containsInOrder xs ys
  | otherwise = containsInOrder (x : xs) ys
